package vultura.fastfactors.inference

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
package object conditioned {
  type Condition = Map[Int,Int]
  type GCondition = Map[Int,Set[Int]]
  implicit class RichCondition(val c: Condition) extends AnyVal {
    def limit(scope: Set[Int]): Condition = c.filterKeys(scope)
    def isCompatibleWith(other: Condition): Boolean = c.keySet.intersect(other.keySet).forall(v => c(v) == other(v))
  }
  def printCondition(c: Condition): String = f"[${c.toSeq.sortBy(_._1).map(kv => kv._1+"="+kv._2).mkString(",")}]"
}
