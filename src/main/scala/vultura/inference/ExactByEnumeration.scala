package vultura.inference

import vultura.factor.inference.MargParI
import vultura.factor.{Factor, LogD, Problem, Ring}
import vultura.util.CrossProductIndexer

/** Exact inference of marginal probabilities and partition function by iterating over all assignments.
  * Runtime complexity of `O(|D|^n)`, where `|D|` is maximum domain size and `n` is number of variables.
  */
case class ExactByEnumeration(problem: Problem) extends MargParI {
  val (logZ: Double, encodedVariableBeliefs: Array[Array[Double]]) = {
    var z = 0d
    val beliefs = problem.domains.map(Array.fill(_)(0d))
    val r = problem.ring
    //iterate over all assignments
    new CrossProductIndexer(problem.domains).foreach{assignment =>
      val value = r.prodA(problem.factors.map(f => f.eval(f.variables.map(assignment), problem.domains))(collection.breakOut))
      z = r.sum(z,value)
      problem.variables.foreach{vi =>
        beliefs(vi)(assignment(vi)) = r.sum(value,beliefs(vi)(assignment(vi)))
      }
    }
    //normalize marginals
    problem.variables.foreach(vi =>
      r.normalizeInplace(beliefs(vi))
    )
    (if(r == LogD) z else math.log(z),beliefs)
  }
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def encodedVarBelief(variable: Int): Factor = Factor(Array(variable),encodedVariableBeliefs(variable))

  override def ring: Ring[Double] = problem.ring
}
