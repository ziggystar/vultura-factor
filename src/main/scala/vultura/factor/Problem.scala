package vultura.factor

import java.io._

import vultura.factor.inference.VariableElimination
import vultura.util.SSet
import vultura.util.TreeWidth._
import vultura.util.graph.Tree

import scala.util.Random

trait BasicProblem {
  def domains: Array[Int]
  def factors: IndexedSeq[Factor]
  def ring: Ring[Double]
}

/** A problem is basically a collection of factors, together with a domain and a ring.
  * It provides several inference methods based on the exact junction tree algorithm. */
case class Problem(factors: IndexedSeq[Factor], domains: Array[Int], ring: Ring[Double]) extends BasicProblem {
  val numVariables = domains.size

  private val degrees: Array[Int] = {
    val r = new Array[Int](numVariables)
    for{
      f <- factors
      v <- f.variables
    } {
      r(v) = r(v) + 1
    }
    r
  }

  lazy val factorsOfVariable: Array[Array[Factor]] = {
    val result: Array[Array[Factor]] = degrees.map(new Array[Factor](_))
    val counter = new Array[Int](numVariables)
    var i = 0
    while(i < factors.size){
      var ii = 0
      val f = factors(i)
      val vs = f.variables
      while(ii < vs.size){
        val v = vs(ii)
        result(v)(counter(v)) = f
        counter(v) = counter(v) + 1
        ii = ii + 1
      }
      i = i + 1
    }
    result
  }

  def variableRange: Range = 0 until numVariables
  def variables: Set[Int] = (0 until numVariables).toSet

  /** @return The set of all neighbouring variables for a given variable, excluding itself. */
  lazy val neighboursOf: Map[Int,Set[Int]] =
    variables.map(v => v -> (factorsOfVariable(v).flatMap(_.variables).toSet - v))(collection.breakOut)

  def filter(p: Factor => Boolean): Problem = this.copy(factors=factors.filter(p))
  def map(p: Factor => Factor): Problem = this.copy(factors=factors.map(p))

  def degreeOfVariable(v: Int): Int = degrees(v)
  def uaiString: String = {
    require(variables == Seq.range(0,variables.size).toSet)
    Seq[Any](
      "MARKOV",
      variables.size,
      domains.mkString(" "),
      factors.size,
      factors.map(f => f.variables.size + " " + f.variables.mkString(" ")).mkString("\n"),
      factors.map(f => f.values.size + " " + ring.decode(f.values).mkString(" ")).mkString("\n")
    ).mkString("\n")
  }

  @deprecated("get rid of this","18")
  def minDegreeJunctionTrees(random: Random): Seq[Tree[(Set[Int], Seq[Factor])]] =
    compactJTrees(minDegreeJTs(factors.map(f => f.variables.toSet -> f)))

  override def hashCode = {
    import scala.util.hashing.MurmurHash3._
    val mix1: Int = mix(arrayHash(domains), orderedHash(factors))
    val mix2: Int = mixLast(mix1, ring.hashCode())
    finalizeHash(mix2, factors.size)
  }
  override def equals(obj: Any): Boolean = obj match {
    case Problem(oFactors, oDomains, oRing) => factors == oFactors && domains.deep == oDomains.deep && ring == oRing
    case _ => false
  }

  lazy val hasDuplicateFactors = factors.size != factors.toSet.size

  def toRing(newRing: Ring[Double]): Problem = Problem(factors.map(f => newRing.encode(ring.decode(f))),domains,newRing)

  def toBriefString: String = f"(Problem: ${variables.size} variables, ${factors.size} factors, ring: $ring"

  /** @return Exact log Z obtained by junction tree algorithm. */
  lazy val logZ: Double = VariableElimination(this).logZ
  
  /** merges factors into other factors where possible */
  def simplify: Problem = {
    val sset: SSet[Int] = new SSet(factors.map(_.variables.toSet)(collection.breakOut))
    val groupedFactors: Iterator[IndexedSeq[Factor]] =
      factors.groupBy(f => sset.maximalSuperSetsOf(f.variables.toSet).maxBy(_.size)).valuesIterator
    val aggregatedFactors: IndexedSeq[Factor] = groupedFactors.map(Factor.multiply(ring)(domains)).toIndexedSeq
    copy(factors = aggregatedFactors)
  }

  /** Set some variables to values and simplify the problem.
    * @param condition Maps variables to the values they shall assume.
    * @return The resulting problem. It will contain a constant representing the product over the now assigned factors.
    */
  def condition(condition: Map[Var,Val]): Problem = {
    val newP = map(_.condition(condition,domains))

    newP.copy(factors = newP.factors ++ condition.map(kv => Factor.deterministicMaxEntropy(Array(kv._1),Map(kv),domains,ring)))
  }

  def fixUncoveredVariables: Problem = {
    val newFactors = for{
      v <- variables if degrees(v) == 0
    } yield Factor.deterministicMaxEntropy(Array(v),Map(),domains, ring)
    this.copy(factors = factors ++ newFactors)
  }

  def hasUncoveredVariable: Boolean = degrees.contains(0)
}

object Problem{
  def fromProblem(bp: BasicProblem): Problem = Problem(bp.factors, bp.domains, bp.ring)
  def fromUaiString(s: String): Problem = parseUAIProblem(new StringReader(s)).right.get
  def loadUaiFile(s: String): Either[String,Problem] = loadUaiFile(new File(s))
  def loadUaiFile(f: File): Either[String,Problem] = {
    val reader = new FileReader(f)
    try {
      parseUAIProblem(reader)
    } catch  {
      case e: Exception => Left("Error while reading file: \n " + e.toString)
    } finally {
      reader.close()
    }
  }

  def parseUAIProblem(in: InputStream): Either[String, Problem] = parseUAIProblem(new InputStreamReader(in))
  /** @return first: the parsed problem; second: true if the input was a bayeschen network ("BAYES"). */
  def parseBayesOrMarkov(in: Reader): Either[String,(Problem,Boolean)] = {
    import scalaz._
    val tokenStream = new BufferedReader(in)

    val lines = Iterator.continually(tokenStream.readLine)
      .takeWhile(_ != null)

    val tokens: Iterator[String] = lines
      .flatMap(_.split(Array(' ','\t','\r')))
      .filterNot(_.isEmpty)

    //first token must be 'MARKOV'
    val asVal: Validation[String, (Problem, Boolean)] = for{
      problemType <- Success(tokens.next().toUpperCase)
      isBayes <- problemType match {
        case "BAYES" => Success(true)
        case "MARKOV" => Success(false)
        case _ => Failure("problem file must start with 'BAYES' or 'MARKOV'")
      }
      numVars = tokens.next().toInt
      domains: Array[Int] = Array.fill(numVars)(tokens.next().toInt)
      numFactors = tokens.next().toInt
      factorVars: Seq[Array[Int]] = Seq.fill(numFactors){
        val nv = tokens.next().toInt
        Array.fill(nv)(tokens.next().toInt)
      }
      factorValues: Seq[Array[Double]] = Seq.fill(numFactors){
        val nv = tokens.next().toInt
        Array.fill(nv)(tokens.next().toDouble)
      }
      factors = (factorVars,factorValues).zipped.map{case (vars,values) => Factor.orderIfNecessary(vars.reverse,values,domains)}
    } yield (Problem(factors.toIndexedSeq,domains,NormalD), isBayes)

    asVal.toEither
  }
  def parseUAIProblem(in: Reader): Either[String,Problem] = parseBayesOrMarkov(in).right.map(_._1)
}