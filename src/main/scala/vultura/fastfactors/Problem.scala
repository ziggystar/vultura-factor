package vultura.fastfactors

import scala.collection.mutable
import scala.util.Random
import vultura.util.TreeWidth._
import scalaz.Tree
import java.io._
import vultura.fastfactors.algorithms.CalibratedJunctionTree
import vultura.util.SSet

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/14/13
 */
case class Problem(factors: IndexedSeq[FastFactor],domains: Array[Int],ring: RingZ[Double]){
  lazy val factorsOfVariable: collection.Map[Int,IndexedSeq[FastFactor]] =
    variables.map(v => v -> factors.filter(_.variables.contains(v)))(collection.breakOut): mutable.HashMap[Int, IndexedSeq[FastFactor]]
  lazy val variables: Set[Int] = (for (f <- factors; v <- f.variables) yield v)(collection.breakOut)

  /** @return The set of all neighbouring variables for a given variable, excluding itself. */
  lazy val neighboursOf: Map[Int,Set[Int]] =
    variables.map(v => v -> (factorsOfVariable(v).flatMap(_.variables).toSet - v))(collection.breakOut)

  def filter(p: FastFactor => Boolean): Problem = this.copy(factors=factors.filter(p))
  def map(p: FastFactor => FastFactor): Problem = this.copy(factors=factors.map(p))

  private lazy val degrees: mutable.HashMap[Int,Int] = new mutable.HashMap[Int,Int]
  def degreeOfVariable(v: Int): Int = degrees.getOrElseUpdate(v,factors.count(_.variables.contains(v)))
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

  def minDegreeJunctionTrees(random: Random): Seq[Tree[(Set[Int], Seq[FastFactor])]] =
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

  def toRing(newRing: RingZ[Double]): Problem = Problem(factors.map(f => newRing.encode(ring.decode(f))),domains,newRing)

  def toBriefString: String = f"(Problem: ${variables.size} variables, ${factors.size} factors, ring: $ring"

  /** @return Exact log Z obtained by junction tree algorithm. */
  def logZ: Double = CalibratedJunctionTree.logZ(this)
  
  /** merges factors into other factors where possible */
  def simplify: Problem = {
    val variablesToFactors: Map[Set[Int], IndexedSeq[FastFactor]] = factors.groupBy(_.variables.toSet)
    val sSet: SSet[Int] = new SSet(variablesToFactors.keySet)
    val maximalSets: Set[Set[Int]] = sSet.maximalSets
    val maximalSetToSets: Map[Set[Int], Iterable[Set[Int]]] = variablesToFactors.keys.groupBy(sSet.superSetsOf(_).intersect(maximalSets).head)
    val values: Map[Set[Int], FastFactor] = maximalSetToSets.mapValues(sets => FastFactor.multiply(ring)(domains)(sets.flatMap(variablesToFactors).toIndexedSeq))
    Problem(values.values.toIndexedSeq, domains, ring)
  }
}

object Problem{
  import resource._

  def fromUaiString(s: String): Problem = parseUAIProblem(new StringReader(s)).right.get
  def loadUaiFile(s: String): Either[String,Problem] = loadUaiFile(new File(s))
  def loadUaiFile(f: File): Either[String,Problem] =
    (for(reader <- managed(new FileReader(f))) yield parseUAIProblem(reader))
      .either.fold(err => Left(err.mkString("\n")),identity)

  def parseUAIProblem(in: InputStream): Either[String, Problem] = parseUAIProblem(new InputStreamReader(in))
  def parseUAIProblem(in: Reader): Either[String,Problem] = {
    import scalaz._
    val tokenStream = new BufferedReader(in)

    val lines = Iterator.continually(tokenStream.readLine)
      .takeWhile(_ != null)

    val tokens: Iterator[String] = lines
      .flatMap(_.split(Array(' ','\t','\r')))
      .filterNot(_.isEmpty)

    //first token must be 'MARKOV'
    val asVal: Validation[String, Problem] = for{
      _ <- if(tokens.next().toUpperCase.matches("MARKOV")) Success() else Failure("file must begin with 'MARKOV'")
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
      factors = (factorVars,factorValues).zipped.map{case (vars,values) => FastFactor.orderIfNecessary(vars.reverse,values,domains)}
    } yield Problem(factors.toIndexedSeq,domains,NormalD)

    asVal.toEither
  }
}