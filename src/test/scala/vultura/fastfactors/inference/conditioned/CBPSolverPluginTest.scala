package vultura.fastfactors.inference.conditioned

import org.specs2._
import org.specs2.specification.Fragments
import vultura.fastfactors.LogD
import vultura.fastfactors.generators._

class CBPSolverPluginTest extends Specification {
  def p1 = grid(6,6,2,expGauss(.2))
  def largeGrid = grid(24,24,2,expGauss(5)).toRing(LogD)
  def loop = grid(2,2,2,expGauss(1))

  override def is: Fragments =
    "CBP 0 has to yield close to exact logZ" ! (new ConditionedInference(p1)().logZ must beCloseTo(p1.logZ,1e-3)) ^
    "CBP 1 has to yield close to exact logZ" ! (new ConditionedInference(p1,runInitially = 1)().logZ must beCloseTo(p1.logZ,1e-3)) ^
    "CBP 2 has to have 2 iteration" ! (new ConditionedInference(p1,runInitially = 2)().iterations === 2) ^
    "CBP 100 has to yield close to exact logZ" ! (new ConditionedInference(p1,runInitially = 100)().logZ must beCloseTo(p1.logZ,1e-5)) ^
    "one step on loop must yield exact result" ! (new ConditionedInference(loop, runInitially = 1)().isExact must beTrue) ^
    "one step on loop must yield exact result" ! (new ConditionedInference(loop, runInitially = 1)().logZ === loop.logZ)
}
