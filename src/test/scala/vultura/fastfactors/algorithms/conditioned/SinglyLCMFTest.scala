package vultura.fastfactors.algorithms.conditioned

import org.specs2.Specification
import org.specs2.specification.Fragments
import scala.util.Random
import vultura.fastfactors._
import vultura.fastfactors.algorithms.CalibratedJunctionTree
import vultura.fastfactors.generators

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class SinglyLCMFTest extends Specification with FastFactorMatchers {
  val singleVarProblem = factoredProblem(1)
  val fourVarProblem = factoredProblem(4)
  def factoredProblem(size: Int): Problem = generators.factorized(size,2,generators.expGauss(1),new Random(1))
  def unconditionedScheme(p: Problem): SimpleScheme = SimpleScheme(p,Set())
  def unconditionedMF(p: Problem): SinglyLCMF = new SinglyLCMF(p, unconditionedScheme(p), 1e-9, 50)

  def is: Fragments =
    "create initial marginal for unconditioned problem" ! {
      unconditionedMF(singleVarProblem).createInitialMarginal(0,Map()) === FastFactor(Array(0),Array(0.5,0.5))
    } ^
    "lcmf mean field is exact for factorized problems" ! (unconditionedMF(fourVarProblem).Z must beCloseTo(new CalibratedJunctionTree(fourVarProblem).Z,0.01)) ^
    "unconditioned lcmf on single variable problem" ! (unconditionedMF(singleVarProblem).Z must beCloseTo(new CalibratedJunctionTree(singleVarProblem).Z,0.01))

}
