package vultura.fastfactors

import scala.collection.mutable
import vultura.factors.uai
import scala.util.Random
import vultura.util.TreeWidth._
import scalaz.Tree

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/14/13
 */
case class Problem(factors: IndexedSeq[FastFactor],domains: Array[Int],ring: RingZ[Double]){
  lazy val variables: Set[Int] = (for (f <- factors; v <- f.variables) yield v)(collection.breakOut)

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
}

object Problem{
  def fromUaiString(s: String): Problem = {
    val inProblem =  uai.parseUAIMarkov(s)

    val domains: Array[Int] = {
      val varDomainMap: Seq[(Int, Int)] =
        inProblem.flatMap(f => f.variables.zip(f.domains.map(_.size)))
      assert(varDomainMap.toMap.size == varDomainMap.distinct.size, "differing variable domains")
      val maxvar = varDomainMap.map(_._1).max
      (0 to maxvar map varDomainMap.toMap.withDefaultValue(0))(collection.breakOut)
    }

    val ff: IndexedSeq[FastFactor] = inProblem
        .toIndexedSeq
        .map(f => FastFactor.orderIfNecessary(f.variables,f.denseData,domains))
        .map{ case FastFactor(vars, values) => FastFactor(vars, values)}
    Problem(ff,domains,NormalD)
  }
}