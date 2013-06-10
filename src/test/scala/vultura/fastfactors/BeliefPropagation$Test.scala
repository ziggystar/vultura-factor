package vultura.fastfactors

import org.specs2.Specification
import org.specs2.specification.Fragments
import Utils._
import BeliefPropagation._

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/10/13
 */
class BeliefPropagation$Test extends Specification {
  def is: Fragments =
  "constructing bethe graph" ^
    "from one factor over single variable" ^
      "must have correct clusters" !
        (createBetheClusterGraph(IS(FF(AI(0),AD(1,2))),AI(2)).clusters.deep === AAI(AI(0),AI(0)).deep) ^
      "must have correct neighbours" !
        (createBetheClusterGraph(IS(FF(AI(0),AD(1,2))),AI(2)).neighbours.deep === AAI(AI(1),AI(0)).deep) ^
      "must have correct neighbours" !
        (createBetheClusterGraph(IS(FF(AI(0),AD(1,2))),AI(2)).neighbours.deep === AAI(AI(1),AI(0)).deep) ^
      "must have correct sepsets" !
        (createBetheClusterGraph(IS(FF(AI(0),AD(1,2))),AI(2)).sepsets.mapValues(_.toSeq) === Map((0,1) -> Seq(0),(1,0) -> Seq(0))) ^
      p^
  p^
  "tests of BP" ^
  "BP on two independent variables" !
    (loopyBeliefPropagation(IS(FF(AI(0),AD(1,2)),FF(AI(1),AD(3,4))),NormalD,Array(2,2)) must beCloseTo(21d,1e-5)) ^
  "BP on one var with two factors" !
    (loopyBeliefPropagation(IS(FF(AI(0),AD(1,2)),FF(AI(0),AD(3,4))),NormalD,Array(2)) must beCloseTo(11d,1e-5))
}
