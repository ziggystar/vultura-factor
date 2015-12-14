package vultura.calibration

import org.specs2.mutable.Specification
import vultura.factor.inference.calibration.LBP
import vultura.factor.{Factor, NormalD, FactorMatchers, Problem}
import vultura.factor.generation._
import vultura.factor.inference.{ConvergenceStats, VariationalResult, RegionBeliefs}
import vultura.inference.gbp.{RgDiagnosis, TwoLayerOC}

import scala.util.Random

/**
  * Created by thomas on 11.12.15.
  */
class TwoLayerOCPropagationTest extends Specification with FactorMatchers {

  val p1: Problem =
    problemGenerator(Generator.only(graph.lattice(4 -> false, 4 -> false))).generate(new Random(0)).problem.toRing(NormalD)

  val t1: Problem =
    problemGenerator(graph.randomTree(50)).generate(new Random(0)).problem.toRing(NormalD)

  val tree_small: Problem =
    problemGenerator(graph.randomTree(4)).generate(new Random(0)).problem.toRing(NormalD)

  def jt(p: Problem): (RegionBeliefs[TwoLayerOC#TLR] with VariationalResult, ConvergenceStats) = {
    val rg = TwoLayerOC.junctionTreeMinDegree(p)
    val prop = new TwoLayerOCPropagation(rg, p.ring)
    Calibrator.calibrateParam(prop, p.factors, 1)
  }

  "test junction-tree on 4x4" >> {
    val result = jt(p1)
    (result._2.isConverged must beTrue) and
      (result._1 must haveExactZ(p1)) and
      (result._1 must haveExactMarginals(p1))
  }

  "test junction-tree on tree with 50 vars" >> {
    val result = jt(t1)
    (result._2.isConverged must beTrue) and
      (result._1 must haveExactZ(t1)) and
      (result._1 must haveExactMarginals(t1))
  }

  "test junction-tree tree with 4 vars" >> {
    val result = jt(tree_small)
    (result._2.isConverged must beTrue) and
      (result._1 must haveExactZ(tree_small)) and
      (result._1 must haveExactMarginals(tree_small))
  }
}
