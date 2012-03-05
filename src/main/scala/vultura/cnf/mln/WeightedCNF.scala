package vultura.cnf.mln

import vultura.cnf.CNF
import vultura.factors.{DenseFactor, Factor}

/**
 * @author Thomas Geier
 * @since 04.03.12
 */

case class WeightedCNF(cnf: CNF, weight: Double)

object WeightedCNF {
  implicit val wcnfAsFactor: Factor[WeightedCNF,Double] = new DenseFactor[WeightedCNF,Double] {
    def variables(f: WeightedCNF): Array[Int] = CNF.variablesOfCNF(f.cnf)
    def domains(f: WeightedCNF): Array[Array[Int]] = CNF.domainOfCNF(f.cnf)
    def evaluate(f: WeightedCNF, assignment: Array[Int]): Double = {
      val (trues,falses) = assignment.zip(variables(f)).partition(_._1 == 1)
      val evalResult = CNF.evaluateCNF(f.cnf,trues.map(_._2),falses.map(_._2))
      evalResult match {
        case None => sys.error("evaluation of factor did not assign every variable")
        case Some(true) => f.weight
        case Some(false) => 1d
      }
    }

    def condition(f: WeightedCNF, variables: Array[Int], values: Array[Int]): WeightedCNF = {
      val (truesTuple: Array[(Int, Int)],falsesTuple: Array[(Int, Int)]) = variables.zip(values).partition(_._2 == 1)
      val (trues, falses) = (truesTuple.map(_._1), falsesTuple.map(_._1))
      f.copy(cnf = CNF(f.cnf.clauses.map(cl => CNF.conditionClause(cl,trues,falses))))
    }
  }
}

