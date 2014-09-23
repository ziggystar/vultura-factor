package vultura.factor.inference.conditioned.lcbp

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor.Problem
import vultura.factor.generators._
import vultura.factor.inference.BeliefPropagation
import vultura.factor.inference.conditioned.GridProblem

import scala.util.Random

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class LCBPTest extends Specification {
  val p1 = grid(2,2,2,expGauss(0.5))
  def fullyConditioned(p: Problem, cv: Int) = GScheme(p.domains, p.variables.map(v => v -> LScheme.split(cv,p.domains)).toMap)
  val p2 = grid(4,4,2,expGauss(1))
  def slightlyConditioned(p: Problem, cv: Int) = GScheme(p.domains, p.neighboursOfVariableInc(cv).map(v => v -> LScheme.split(cv,p.domains)).toMap)

  val rand1 = randomK(10,6,3,2,expGauss(1))

  val overlappingGrid = GridProblem(6,1,2,1d,4)
  val overlappingGridSmall = GridProblem(width = 3,margin=0,influence=2,coupling = 1d,numConditioned = 4)

  override def is: Fragments =
    "unconditioned" ^
      "yield same result as BP" ^
        "on simple loop" ! (new LCBP(p1,GScheme(p1.domains),1e-9,10000).logZ must beCloseTo(new BeliefPropagation(p1,new Random(0),1e-7,10000).logZ,0.0001)) ^
        "on less simple loop" ! (new LCBP(p2,GScheme(p2.domains),1e-9,10000).logZ must beCloseTo(new BeliefPropagation(p2,new Random(0),1e-7,10000).logZ,0.0001)) ^
      p^
    p^
    "fully conditioned" ^
      "2x2 grid is exact" ! convergedAndExactTo(new LCBP(p1,fullyConditioned(p1,0)), 1e-6) ^
      p^
    "locally conditioned" ^
      "grid" ! convergedAndExactTo(new LCBP(p2,slightlyConditioned(p2,0), maxIterations = 100000),5e-3).orSkip ^
      "overlapping influences" ^
      "bug, threw an exception" ! convergedAndExactTo(new LCBP(overlappingGrid.problem,overlappingGrid.gscheme, maxIterations = 100000),0.1) ^
      "small grid" ! convergedAndExactTo(new LCBP(overlappingGridSmall.problem,overlappingGridSmall.gscheme, maxIterations = 100000),0.1) ^
    p^
    "exactness on trees" ^
      "chain l=5" ! {
        val p = grid(3,1)
        new LCBP(p,FactoredScheme(p,Map(1->Set(1))).toGScheme).logZ must beCloseTo(p.logZ, 1e-6)
      }.orSkip

  def convergedAndExactTo(lcbp:LCBP, tol: Double) = (lcbp.calibrator.isConverged must beTrue) and (lcbp.logZ must beCloseTo(lcbp.problem.logZ, tol))
}
