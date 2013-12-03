package vultura.fastfactors.algorithms

import org.specs2._
import org.specs2.specification.Fragments
import vultura.fastfactors.SampleProblems

/**
 * These tests compare the results on the example problems to the results of a different solver
 * (libdai entry to competition). Note that external competition solvers return result in log-10.
 */
class UaiExampleProblemInference extends Specification {
  def is: Fragments =
    "compare ground truth of uai examples to results of calibrated JT" ^
      calibratedJTTest

  def calibratedJTTest = Fragments.create(
    SampleProblems.examples.filter(_.logZ.isDefined).map( example =>
      example.filename ! (CalibratedJunctionTree.logZ(example.problem) must beCloseTo(example.logZ.get,0.01))
      ):_*)
}