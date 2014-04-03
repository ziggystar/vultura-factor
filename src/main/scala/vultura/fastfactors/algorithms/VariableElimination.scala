package vultura.fastfactors.algorithms

import vultura.fastfactors.{FastFactor, Problem}

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
case class VariableElimination(problem: Problem, orderer: VariableOrderer = MinDegreeOrderer) extends ParFunI {
  val variableOrder = orderer(problem)

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = {
    val resultFactors = variableOrder.foldLeft(problem.factors){case (factors, elimVar) =>
      val (elimFactors, remaining) = factors.partition(_.variables.contains(elimVar))
      val elimResult = FastFactor.multiplyMarginalize(problem.ring)(problem.domains)(elimFactors,Array(elimVar))
      remaining :+ elimResult
    }
    require(resultFactors.forall(_.variables.isEmpty), "order did not cover all variables")
    problem.ring.prodA(resultFactors.map(_.values(0))(collection.breakOut))
  }
}
