package vultura.wsat

import vultura.util.IntDomainCPI

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 3/15/13
 */
case class WeightedKB(variables: Set[Int], clauses: Seq[WeightedClause]){
  def sumProductBrute: Double = {
    def eval(assignment: Map[Int,Int]) = {
      clauses.map(_.eval(assignment)).product
    }
    val ordering = variables.toArray
    val assignments = new IntDomainCPI(Array.fill(ordering.size)(Array(0,1)))
    assignments.foldLeft(0d){case (p,ass) => p + eval(ordering.zip(ass).toMap)}
  }
}
