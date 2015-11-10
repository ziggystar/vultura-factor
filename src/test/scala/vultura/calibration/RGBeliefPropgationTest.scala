package vultura.calibration

import org.specs2.mutable.Specification
import vultura.factor.inference.calibration.{LBP, BPResult}
import vultura.factor.inference.gbp.{OvercountingRegionGraph, RegionGraph}
import vultura.factor.{FactorMatchers, LogD, Problem}
import vultura.factor.generators._

import scala.util.Random

class RGBeliefPropgationTest extends Specification with FactorMatchers {
  val p1: Problem = grid(5,5,4).simplify.toRing(LogD)
  val p1_normal: Problem = grid(5,5,4).simplify

  "compare PTC on bethe RG with ordinary LBP result" >> {
    val regularBPResult: BPResult = LBP.infer(p1,tol=1e-15)

    val (ptcResult, status) = Calibrator.calibrate(RGBeliefPropagation(RegionGraph.betheRG(p1),p1),tol=1e-15)

    status.isConverged and (ptcResult must haveSameMarginals(LBP.infer(_,tol=1e-15),1e-12))
  }

  "compare PTC on bethe RG with ordinary LBP result (normal encoding)" >> {
    val regularBPResult: BPResult = LBP.infer(p1_normal,tol=1e-15)

    val (ptcResult, status) = Calibrator.calibrate(RGBeliefPropagation(RegionGraph.betheRG(p1_normal),p1_normal),tol=1e-15)

    status.isConverged and (ptcResult must haveSameMarginals(LBP.infer(_,tol=1e-15),1e-12))
  }

  "redundant region graphs" >> {
    import vultura.factor.generation._
    val grid3x3: LabeledProblem[IndexedSeq[Int]] =
      problemGenerator(Generator.only(graph.lattice(3 -> false, 3 -> false))).generate(new Random(0))
    val problem = grid3x3.problem
    val centerVar = grid3x3.variableLabels.forward(IndexedSeq(1,1))
    val betheRegions = problem.scopeOfFactor.map(_.toSet) ++ problem.variables.map(Set(_))
    val conditionRG = OvercountingRegionGraph(
      problem,
      betheRegions.map(_ + centerVar)(collection.breakOut),
      problem.scopeOfFactor.zipWithIndex.map{case (sc,fi) => (fi, sc.toSet + centerVar)}(collection.breakOut)
    )

    "have valid marignals as result" >> {
      val p2c = RGBeliefPropagation(conditionRG, problem)

      val (rgResult, status) = Calibrator.calibrate(p2c, damping = 0.3)
      (status.isConverged.aka("rgBP is converged") must beTrue) and
        (rgResult must haveValidMarginals)
    }
  }

}
