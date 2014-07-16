package vultura.factor.inference.calibration

import org.specs2.Specification
import vultura.factor.generators._
import scala.util.Random
import org.specs2.specification.Fragments
import vultura.factor.{LogD, FastFactorMatchers}
import vultura.factor.inference.{BeliefPropagation, JunctionTree}

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class BP_CalTest extends Specification with FastFactorMatchers{
  val treeProb = treeK(10,3,2,expGauss(1),new Random(0))
  val treeProbLog = treeProb.toRing(LogD)

  val smoothGrid = grid(6,6,2,expGauss(0.1), new Random(0))

  override def is: Fragments =
    "must be exact on tree" ! (new BP_Cal(treeProb).logZ must beCloseTo(new JunctionTree(treeProb).logZ, 0.01)) ^
    "must be exact on tree (log)" ! (new BP_Cal(treeProbLog).logZ must beCloseTo(new JunctionTree(treeProb).logZ, 0.01)) ^
    "must be nearly exact on smooth grid" ! (new BP_Cal(smoothGrid).logZ must beCloseTo(new JunctionTree(smoothGrid).logZ,0.01))
}