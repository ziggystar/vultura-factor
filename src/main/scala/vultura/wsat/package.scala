package vultura

import vultura.factors.TableFactor
import vultura.util.RingWithZero

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 3/15/13
 */
package object wsat {
  /** @return A weighted knowledge base, such that for each assignment the product of the weights of all satisfied
    * clauses equals the value of the factor product for that assignment.
    * @param p
    */
  def productToClauseWeightedKB(p: Seq[TableFactor[Double]]): WeightedKB = {
    require(p.forall(_.independentVariables.isEmpty), "compilation to WSAT cannot handle independent variables")
    require(p.forall(_.domains.forall(_.toSeq == Seq(0,1))), "compilation to WSAT can only handly binary variables")
    val clauses = for(
      factor <- p;
      numAssignments = factor.cpi.size;
      localZ = math.pow(factors.partition(factor,RingWithZero.sumProduct.multiplication),1d/(numAssignments-1)); //on each assignment, all but one clauses will be true
      assignment <- factor.cpi
    ) yield WeightedClause(factor.variables.zip(assignment).map(t => Literal(t._1,if(t._2==0) 1 else 0)).toSet,localZ/factor.evaluate(assignment))
    WeightedKB(p.flatMap(_.variables).toSet,clauses)
  }
}
