package vultura.fastfactors

import org.specs2.Specification
import org.specs2.specification.Fragments
import Utils._
import vultura.fastfactors.algorithms.BeliefPropagation

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/10/13
 */
class BeliefPropagation$Test extends Specification {
  def BpLogZ(factors: IndexedSeq[FastFactor], ring: RingZ[Double], domains: Array[Int]): Double = {
    val bp = new BeliefPropagation(Problem(factors,domains,ring))
    bp.run(10000,1e-10)
    bp.logZ
  }

  def is: Fragments =
  "constructing bethe graph" ^
    "from one factor over single variable" ^
      "must have correct neighbours" !
        (BeliefPropagation.createBetheClusterGraph(IS(FF(AI(0),AD(1,2))),AI(2),NormalD).neighbours.deep === AAI(AI(1),AI(0)).deep) ^
      "must have correct neighbours" !
        (BeliefPropagation.createBetheClusterGraph(IS(FF(AI(0),AD(1,2))),AI(2),NormalD).neighbours.deep === AAI(AI(1),AI(0)).deep) ^
      "must have correct sepsets" !
        (BeliefPropagation.createBetheClusterGraph(IS(FF(AI(0),AD(1,2))),AI(2),NormalD).sepsets.mapValues(_.toSeq) === Map((0,1) -> Seq(0),(1,0) -> Seq(0))) ^
      p^
  p^
  "tests of BP" ^
  "BP on two independent variables" !
    (BpLogZ(IS(FF(AI(0),AD(1,2)),FF(AI(1),AD(3,4))),NormalD,Array(2,2)) must beCloseTo(math.log(21d),1e-8)) ^
  "BP on one var with two factors" !
    (BpLogZ(IS(FF(AI(0),AD(1,2)),FF(AI(0),AD(3,4))),NormalD,Array(2)) must beCloseTo(math.log(11d),1e-8))
}
