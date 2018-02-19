package vultura.inference

import org.specs2.mutable.Specification
import vultura.factor.inference.calibration.LBP
import vultura.factor.{FactorMatchers, NormalD, Problem, generation}

import scala.util.Random

class BetheProblemTest extends Specification with FactorMatchers {
  import vultura.factor.generation._

  val tree = problemGenerator(generation.graph.randomTree(10),param = IIDValuedParam(Generator.uniform(-3,3))).generate(new Random(0)).problem.toRing(NormalD)
  val grid6x6 = problemGenerator(Generator.only(generation.graph.lattice(6 -> false, 6 -> false))).generate(new Random(0)).problem

  def beliefPropagation = ((p: Problem) => BeliefPropagation.infer(p, damping = 0)).aka("belief propagation")

  "bethe approximation yields exact result on tree" >> {
    val (result,stats) = BeliefPropagation.infer(tree,damping = 0)
    (stats.isConverged must beTrue) and
      (result must haveExactZ(tree)) and
      (result must haveExactMarginals(tree,1e-12))
  }

  "compare BetheProblem to old LBP implementation" >> {
    "on tree" >> newBPisSameAsOld(tree)
    "on grid 6x6" >> newBPisSameAsOld(grid6x6)
  }

  def newBPisSameAsOld(p: Problem, maxIt: Int = 100000, convergenceTol: Double = 1e-15, compareTol: Double = 1e-9) = {
    val (newResult,stats) = BeliefPropagation.infer(p,tol = convergenceTol, maxIterations = maxIt)
    val oldBPResult = LBP.infer(p, maxIterations = maxIt, tol=convergenceTol)

    stats.isConverged.aka("new BP is converged") must beTrue
    newResult must haveSameLogZ(oldBPResult,compareTol)
    newResult must haveSameMarginals(oldBPResult, compareTol)
  }
}
