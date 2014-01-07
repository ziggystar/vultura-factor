package vultura.fastfactors.algorithms

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
package object conditioned {
  type Condition = Map[Int,Int]
  implicit class RichCondition(val c: Condition) extends AnyVal {
    def limit(scope: Set[Int]): Condition = c.filterKeys(scope)
    def isCompatibleWith(other: Condition): Boolean = c.keySet.intersect(other.keySet).forall(v => c(v) == other(v))
  }
}
