package vultura.calibration

import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment
import vultura.factor.inference.calibration.LBP
import vultura.factor._
import vultura.factor.generation._
import vultura.factor.inference.gbp.RegionGraph
import vultura.factor.inference.{ConvergenceStats, RegionBeliefs, VariationalResult}
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

  val magnetized1: Problem = {
    val problemStructure = LabeledProblemStructure
      .fromGraph(graph.lattice(4 -> false, 4 -> false), (_: IndexedSeq[Int]) => 2)
      .addSingletons(_ => true)

    IIDValuedParam(Generator.gaussian(0d, 1d)).parameterize(
      problemStructure
    ).generate(new Random(0)).problem
  }

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

  "aggregated vs. asis Bethe region graph construction must yield same results on" >> {
    "when there is no redundancy" >> {
      aggregatedVsAsis(p1.simplify, "lattice 4x4, simplified")
      aggregatedVsAsis(t1.simplify, "tree with 50 nodes, simplified")
    }
    "when there are redundant singleton factors" >> {
      aggregatedVsAsis(magnetized1, "lattice 4x4 with magnetization")
    }
  }

  "junction tree construction must assign factors with empty scope to somewhere" >> {
    val p = generation.pottsGrid(Seq(2 -> false,2 -> false)).withSeed(0).problem
    val withFactor = p.copy(factors = p.factors :+ Factor(Array(), Array(5d)))
    val jt = TwoLayerOC.junctionTreeMinDegree(withFactor)
    jt.regions.flatMap(jt.factorsOf) must containTheSameElementsAs(withFactor.factorIndices)
  }


  /** Aggregated and asis Bethe region graphs should yield equal results (as long as there are no parallel cycles
    * induced by redundant factor scopes. In particular if only singleton factors are redundant (their scope contained
    * within the scope of other factors), the results must be equal.
    * @param p
    * @param name
    * @return
    */
  def aggregatedVsAsis(p: Problem, name: String, maxIterations: Long = 10000, maxDiff: Double = 1e-12, damping: Double = 0d): Fragment = {
    s"$name" ! {
      def calibrate(rg: TwoLayerOC): (RegionBeliefs[TwoLayerOC#TLR], ConvergenceStats) = {
        val cp = new TwoLayerOCPropagation(rg, p.ring)
        Calibrator.calibrateParam(cp, p.factors, maxIterations, tol = maxDiff, damping = damping)
      }
      val (aggr_res, aggr_stats) = calibrate(TwoLayerOC.betheRegionGraph(p,aggregateFactors = true))
      val (asis_res, asis_stats) = calibrate(TwoLayerOC.betheRegionGraph(p,aggregateFactors = false))
      (aggr_stats.isConverged.aka("aggregated is converged") must beTrue) and
        (asis_stats.isConverged.aka("asis is converged") must beTrue) and
        (aggr_res must haveSameMarginals(asis_res, 1e-6))
    }
  }
}
