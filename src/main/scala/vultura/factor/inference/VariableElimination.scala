package vultura.factor.inference

import vultura.factor.{Ring, NormalD, Factor, Problem}

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
case class VariableElimination(problem: Problem, orderer: VariableOrderer = MinDegreeOrderer) extends ParFunI {

  override def ring: Ring[Double] = problem.ring

  val variableOrder = orderer(problem)

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = {
    val resultFactors = variableOrder.order.foldLeft(problem.factors){case (factors, elimVar) =>
      val (elimFactors, remaining) = factors.partition(_.variables.contains(elimVar))
      val elimResult = Factor.multiplyMarginalize(problem.ring)(problem.domains)(elimFactors,Array(elimVar))
      remaining :+ elimResult
    }
    require(resultFactors.forall(_.variables.isEmpty), "order did not cover all variables")
    problem.ring.prodA(resultFactors.map(_.values(0))(collection.breakOut))
  }

  /** @return Natural logarithm of partition function. */
  override def logZ: Double = if(problem.ring == NormalD) math.log(Z) else Z
}
