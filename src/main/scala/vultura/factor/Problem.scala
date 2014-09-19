package vultura.factor

import java.io._

import vultura.factor.inference.VariableElimination
import vultura.util.SSet
import vultura.util.TreeWidth._
import vultura.util.graph.Tree

import scala.util.Random

trait ProblemStructure {
  type VI = Int
  type FI = Int
  /** The domain size for each variable. Variable indices start at zero and are consecutive. */
  def domains: Array[Int]
  def scopeOfFactor: Array[Array[VI]]
  
  val numVariables: Int = domains.size
  val numFactors: Int = scopeOfFactor.size
  
  def variables: Range = 0 until numVariables
  lazy val variableSet: Set[Int] = (0 until numVariables).toSet

  lazy val degrees: Array[Int] = {
    val r = new Array[Int](numVariables)
    for{
      scope <- scopeOfFactor
      vi <- scope
    } {
      r(vi) = r(vi) + 1
    }
    r
  }
  
  /** Indexing with a variable index yields the factor indices this variable participates in. */
  lazy val factorIdxOfVariable: Array[Array[FI]] = {
    val result: Array[Array[Int]] = degrees.map(new Array[Int](_))
    val counter = new Array[Int](numVariables)
    var fi = 0
    while(fi < numFactors){
      val scope = scopeOfFactor(fi)
      var vi = 0
      while(vi < scope.size){
        val v = scope(vi)
        result(v)(counter(v)) = fi
        counter(v) = counter(v) + 1
        vi = vi + 1
      }
      fi = fi + 1
    }
    result
  }
  /** The index with variable index `vi`, yields the neighbours of the variable, without `vi`. */ 
  lazy val neighboursOfVariableEx: Array[Array[VI]] =
    factorIdxOfVariable.zipWithIndex.map{ case (fs,v) => fs.flatMap(fi => scopeOfFactor(fi)).filterNot(_ == v) }
  lazy val neighboursOfVariableInc: Array[Array[VI]] =
    factorIdxOfVariable.zipWithIndex.map{ case (fs,v) => fs.flatMap(fi => scopeOfFactor(fi)) }
}

/** A problem is basically a collection of factors, together with a domain and a ring.
  * It provides several inference methods based on the exact junction tree algorithm. */
case class Problem(factors: IndexedSeq[Factor], domains: Array[Int], ring: Ring[Double]) extends ProblemStructure {

  override def scopeOfFactor: Array[Array[VI]] = factors.map(_.variables)(collection.breakOut)

  def factorsOfVariable(v: Int): Array[Factor] = factorIdxOfVariable(v).map(factors)

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
    val aggregatedFactors: IndexedSeq[Factor] = factors
      .groupBy(f => sset.maximalSuperSetsOf(f.variables.toSet).maxBy(_.size))
      .map{case (_,v) => Factor.multiply(ring)(domains)(v)}(collection.breakOut)
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