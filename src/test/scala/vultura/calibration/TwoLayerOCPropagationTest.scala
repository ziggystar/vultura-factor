package vultura.calibration

import org.specs2.mutable.Specification
import vultura.factor.{FactorMatchers, Problem}
import vultura.factor.generation._
import vultura.factor.inference.{ConvergenceStats, VariationalResult, RegionBeliefs}
import vultura.inference.gbp.{RgDiagnosis, TwoLayerOC}

import scala.util.Random

/**
  * Created by thomas on 11.12.15.
  */
class TwoLayerOCPropagationTest extends Specification with FactorMatchers {

  val p1: Problem =
    problemGenerator(Generator.only(graph.lattice(4 -> false, 4 -> false))).generate(new Random(0)).problem

  def jt(p: Problem): (RegionBeliefs[TwoLayerOC#TLR] with VariationalResult, ConvergenceStats) = {
    val rg = TwoLayerOC.junctionTreeMinDegree(p)
    println(RgDiagnosis(rg).validityIssues.mkString("\n"))
    println(rg.directedGraph.isTree, "jt isn't acyclic")
    rg.toDot.labelNodes{
      case r => r.variables.mkString(",")
    }.renderPDF("jt")
    val prop = new TwoLayerOCPropagation(rg, p.ring)
    Calibrator.calibrateParam(prop, p.factors, 1)
  }

  "test junction-tree" >> {
    val result = jt(p1)
    (result._2.isConverged must beTrue) and
      (result._1 must haveExactZ(p1)) and
      (result._1 must haveExactMarginals(p1))
  }

}
