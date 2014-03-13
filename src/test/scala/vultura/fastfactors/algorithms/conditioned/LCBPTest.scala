package vultura.fastfactors.algorithms.conditioned

import org.specs2.Specification
import org.specs2.specification.Fragments

import vultura.fastfactors.generators._
import scala.util.Random
import vultura.fastfactors.algorithms.{CalibratedJunctionTree, BeliefPropagation}
import vultura.fastfactors.Problem

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class LCBPTest extends Specification {
  val p1 = grid(2,2,2,expGauss(0.5))
  def fullyConditioned(p: Problem, cv: Int) = GScheme(p.domains, p.variables.map(v => v -> LScheme.split(cv,p.domains)).toMap)
  val p2 = grid(4,4,2,expGauss(1))
  def slightlyConditioned(p: Problem, cv: Int) = GScheme(p.domains, (p.neighboursOf(cv) + cv).map(v => v -> LScheme.split(cv,p.domains)).toMap)

  val overlappingGrid = GridProblem(6,1,2,1d,4)
  val overlappingGridSmall = GridProblem(width = 3,margin=0,influence=2,coupling = 1d,numConditioned = 4)

  // todo: create matcher that checks for convergence
  override def is: Fragments =
    "unconditioned" ^
      "yield same result as BP" ^
        "on simple loop" ! (new LCBP(p1,GScheme(p1.domains),1e-7,10000).logZ must beCloseTo(new BeliefPropagation(p1,new Random(0),1e-7,10000).logZ,0.0001)) ^
        "on less simple loop" ! (new LCBP(p2,GScheme(p2.domains),1e-7,10000).logZ must beCloseTo(new BeliefPropagation(p2,new Random(0),1e-7,10000).logZ,0.0001)) ^
      p^
    p^
    "fully conditioned" ^
      {
        new LCBP(p1,fullyConditioned(p1,0)).logZ must beCloseTo(CalibratedJunctionTree.logZ(p1),1e-5)
      } ^
    p^
    "locally conditoned" ^
      {
        new LCBP(p2,slightlyConditioned(p2,0)).logZ must beCloseTo(CalibratedJunctionTree.logZ(p2),1e-3)
      } ^
    "overlapping influences" ^
      "bug, threw an exception" ! {new LCBP(overlappingGrid.problem,overlappingGrid.gscheme, maxIterations = 100000).logZ must beCloseTo(overlappingGrid.problem.logZ,0.1)} ^
      "small grid" ! {new LCBP(overlappingGridSmall.problem,overlappingGridSmall.gscheme, maxIterations = 100000).logZ must beCloseTo(overlappingGridSmall.problem.logZ,0.1)}


}
