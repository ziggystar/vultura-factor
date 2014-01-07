package vultura.fastfactors.algorithms.conditioned

import org.specs2.Specification
import org.specs2.specification.Fragments
import scala.util.Random
import vultura.fastfactors._
import vultura.fastfactors.algorithms.{MeanField, CalibratedJunctionTree}
import vultura.fastfactors.generators

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class SinglyLCMFTest extends Specification with FastFactorMatchers {
  val singleVarProblem: Problem = factoredProblem(1)
  val fourVarProblem: Problem = factoredProblem(4)
  val simpleGrid: Problem = generators.grid(2,2,2,generators.expGauss(1),new Random(1))
  val mediumGrid: Problem = generators.grid(4,4,4,generators.expGauss(1),new Random(3))

  def factoredProblem(size: Int): Problem = generators.factorized(size,2,generators.expGauss(1),new Random(1))
  def unconditionedScheme(p: Problem): SimpleScheme = SimpleScheme(p,Set())
  def unconditionedMF(p: Problem): SinglyLCMF = new SinglyLCMF(p, unconditionedScheme(p), 1e-9, 50)

  def is: Fragments =
    "create initial marginal for unconditioned problem" ! {
      unconditionedMF(singleVarProblem).createInitialMarginal(0,Map()) === FastFactor(Array(0),Array(0.5,0.5))
    } ^
    "inference on unconditioned problems" ^
      "lcmf mean field is exact for factorized problems" ! (unconditionedMF(fourVarProblem).Z must beCloseTo(new CalibratedJunctionTree(fourVarProblem).Z,0.01)) ^
      "unconditioned lcmf on single variable problem" ! (unconditionedMF(singleVarProblem).Z must beCloseTo(new CalibratedJunctionTree(singleVarProblem).Z,0.01)) ^
      "yield same result as normal MF on small grid" ! (unconditionedMF(simpleGrid).Z must beCloseTo(new MeanField(simpleGrid).Z,1e-3)) ^
      "yield same result as normal MF on medium grid" ! (unconditionedMF(mediumGrid).Z must beCloseTo(new MeanField(mediumGrid).Z,1e-3)) ^
    p^
    "inference on conditioned problems" ^
      "singly conditioned single variable problem must yield exact result" ! {
        new SinglyLCMF(singleVarProblem,SimpleScheme(singleVarProblem,Set((Set(0),0)))).Z must beCloseTo(new CalibratedJunctionTree(singleVarProblem).Z,1e-7)
      } ^
      "singly conditioned factorized problem must yield exact result" ! {
        new SinglyLCMF(fourVarProblem,SimpleScheme(fourVarProblem,Set((Set(0,1),0)))).Z must beCloseTo(new CalibratedJunctionTree(fourVarProblem).Z,1e-7)
      } ^
      "singly conditioned factorized problem must yield exact result" ! {
        new SinglyLCMF(fourVarProblem,SimpleScheme(fourVarProblem,Set((Set(0,1,2,3),0)))).Z must beCloseTo(new CalibratedJunctionTree(fourVarProblem).Z,1e-7)
      }



}
