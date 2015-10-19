package vultura.factor.generation

import vultura.factor.{LogD, Problem, Factor}

/** A generator for problem parameters that can be fit on a problem structure. */
trait Parameterization[-N]{
  def generateFactors[L <: N](ps: LabeledProblemStructure[L]): Generator[LabeledProblem[L]]
}

/** Each factor value is distributed iid within the logarithmic domain.
  * This means the generated value gets exponentiated before multiplication.
  * This is equivalent to an energy value for a Boltzmann distribution.
  * @param valueGen Generate values from the real-number line (this means negative values are ok).
  */
case class IIDValuedParam(valueGen: Generator[Double]) extends Parameterization[Any] {
  override def generateFactors[L <: Any](ps: LabeledProblemStructure[L]): Generator[LabeledProblem[L]] = {
    val factors: Generator[Seq[Factor]] = Generator.seq(ps.structure.scopeOfFactor.map(scope =>
      Generator(r => Factor.fromFunction(scope, ps.structure.domains, _ => valueGen.generate(r)))
    ))
    factors.map(fs => LabeledProblem(Problem(fs.toIndexedSeq,ps.structure.domains,LogD), ps.variableLabels))
  }
}