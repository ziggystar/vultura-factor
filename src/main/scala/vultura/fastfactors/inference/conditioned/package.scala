package vultura.fastfactors.inference

import vultura.fastfactors.{FastFactor, Problem}

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
package object conditioned {
  type Condition = Map[Int,Int]
  type GCondition = Map[Int,Set[Int]]

  /** Extracts zeros from a factor, and returns a generalized condition, capturing those. */
  def extractCondition(f: FastFactor, problem: Problem): GCondition = {
    require(f.variables.size == 1)
    val v = f.variables(0)
    val allowedValues: Set[Int] = (0 until problem.domains(v)).filterNot(i => f.values(i) == problem.ring.zero).toSet
    if(allowedValues.size < problem.domains(v))
      Map(v -> allowedValues)
    else
      Map()
  }

  /** @return A factor that enforces a generalized condition that is otherwise neutral. */
  def gCondToFactors(gc: GCondition, p: Problem): IndexedSeq[FastFactor] = gc.map{
    case (v,values) => FastFactor
      .fromFunction(Array(v),p.domains, vs => if(values.contains(vs(0))) p.ring.one else p.ring.zero)
  }(collection.breakOut)

  /** @return A GCondition that fulfills all given conditions (thus the value sets are intersected on a per variable basis). */
  def gcondConjunction(gcs: GCondition*): GCondition =
    Map(gcs.flatMap(_.keySet).map(k => k -> gcs.flatMap(_.get(k)).reduce(_ intersect _)):_*)

  /** Partition a generalized condition into deterministic assignments and remaining generalized conditions. */
  def extractHardConditions(gc: GCondition, p: Problem): (GCondition,Condition) = {
    val (hard,soft) = gc.partition(_._2.size == 1)
    (soft, hard.map{ case (k,vals) => k -> vals.head })
  }

  implicit class RichCondition(val c: Condition) extends AnyVal {
    def limit(scope: Set[Int]): Condition = c.filterKeys(scope)
    def isCompatibleWith(other: Condition): Boolean = c.keySet.intersect(other.keySet).forall(v => c(v) == other(v))
  }
  def printCondition(c: Condition): String = f"[${c.toSeq.sortBy(_._1).map(kv => kv._1+"="+kv._2).mkString(",")}]"
}
