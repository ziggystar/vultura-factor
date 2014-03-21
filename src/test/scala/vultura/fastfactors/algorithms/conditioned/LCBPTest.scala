package vultura.fastfactors.algorithms.conditioned

import org.specs2.Specification
import org.specs2.specification.Fragments

import vultura.fastfactors.generators._
import scala.util.Random
import vultura.fastfactors.algorithms.{CalibratedJunctionTree, BeliefPropagation}
import vultura.fastfactors.Problem
import org.specs2.matcher.MatchResult
import vultura.fastfactors.algorithms.calibration.BP_Cal

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class LCBPTest extends Specification {
  val p1 = grid(2,2,2,expGauss(0.5))
  def fullyConditioned(p: Problem, cv: Int) = GScheme(p.domains, p.variables.map(v => v -> LScheme.split(cv,p.domains)).toMap)
  val p2 = grid(4,4,2,expGauss(1))
  def slightlyConditioned(p: Problem, cv: Int) = GScheme(p.domains, (p.neighboursOf(cv) + cv).map(v => v -> LScheme.split(cv,p.domains)).toMap)

  val rand1 = randomK(10,6,3,2,expGauss(1))

  val overlappingGrid = GridProblem(6,1,2,1d,4)
  val overlappingGridSmall = GridProblem(width = 3,margin=0,influence=2,coupling = 1d,numConditioned = 4)

  // todo: create matcher that checks for convergence
  override def is: Fragments =
    "unconditioned" ^
      "yield same result as BP" ^
        "on simple loop" ! (new LCBP(p1,GScheme(p1.domains),1e-9,10000).logZ must beCloseTo(new BeliefPropagation(p1,new Random(0),1e-7,10000).logZ,0.0001)) ^
        "on less simple loop" ! (new LCBP(p2,GScheme(p2.domains),1e-9,10000).logZ must beCloseTo(new BeliefPropagation(p2,new Random(0),1e-7,10000).logZ,0.0001)) ^
      p^
    p^
    "fully conditioned" ^
      "grid" ! convergedAndExactTo(new LCBP(p1,fullyConditioned(p1,0)), 1e-5) ^
      "random structure" ! convergedAndExactTo(new LCBP(rand1,fullyConditioned(rand1,0),1e-9,10000), 1e-2) ^
      p^
    "locally conditioned" ^
      "grid" ! convergedAndExactTo(new LCBP(p2,slightlyConditioned(p2,0), maxIterations = 100000, exactConditions = false),1e-3).orSkip ^
      "compare lcbp with and without correction" ^
        "4x1 grid" ! {
          val problem = grid(4,1,2,expGauss(1),new Random(1)).simplify
          val scheme = slightlyConditioned(problem, 0)
          lcbpCorrectionAnalysis(problem, scheme, 1e-4, 100000)
        } ^
        "4x1 attractive grid" ! {
          val problem = grid(4,1,2,attractive(4),new Random(1)).simplify
          val scheme = slightlyConditioned(problem, 0)
          lcbpCorrectionAnalysis(problem, scheme, 1e-4, 100000)
        }
      "random structure" ! convergedAndExactTo(new LCBP(rand1,slightlyConditioned(rand1,0),1e-9,10000, exactConditions = false), 1e-2) ^
      "random structure (exact)" ! convergedAndExactTo(new LCBP(rand1,slightlyConditioned(rand1,0),1e-9,10000,exactConditions = true), 1e-2) ^
      "overlapping influences" ^
      "bug, threw an exception" ! convergedAndExactTo(new LCBP(overlappingGrid.problem,overlappingGrid.gscheme, maxIterations = 100000),0.1) ^
      "small grid" ! convergedAndExactTo(new LCBP(overlappingGridSmall.problem,overlappingGridSmall.gscheme, maxIterations = 100000),0.1)


  def lcbpCorrectionAnalysis(problem: Problem, scheme: GScheme, tol: Double, maxIterations: Int): MatchResult[AnyVal] = {
    val lcbp = new LCBP(problem, scheme, tol = 1e-11, maxIterations = 100000, exactConditions = false)
    val lcbpCorrected: LCBP = new LCBP(problem, scheme, maxIterations = 100000, exactConditions = true)
    println("problem:\n\t" + problem.factors.mkString("\n\t"))
    println("# LCBP-corrected")
    println(lcbpCorrected.calibrator.report)
    println("# LCBP-uncorrected")
    println(lcbp.calibrator.report)
    println("\n\ntrue logz: " + problem.logZ)
    println("lcbp-corrected: " + lcbpCorrected.logZ)
    println("lcbp:\t" + lcbp.logZ)
    println(lcbpCorrected.calibrator.isCalibrated + "/" + lcbp.calibrator.isCalibrated)
    (lcbpCorrected.calibrator.isCalibrated must beTrue) and (lcbp.calibrator.isCalibrated must beTrue) and
      (lcbpCorrected.logZ must beCloseTo(lcbp.logZ, 1e-4))
  }

  def convergedAndExactTo(lcbp:LCBP, tol: Double) = (lcbp.calibrator.isCalibrated must beTrue) and (lcbp.logZ must beCloseTo(lcbp.getProblem.logZ, tol))
}
