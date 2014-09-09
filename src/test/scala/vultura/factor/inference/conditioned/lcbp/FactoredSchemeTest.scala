package vultura.factor.inference.conditioned.lcbp

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor.{NormalD, Problem}
import vultura.factor.generators._

class FactoredSchemeTest extends Specification {
  val problem4v = Problem(IndexedSeq(), Array(2,2,2,2), NormalD)
  val singletonScheme4v = FactoredScheme(problem4v, (0 to 3).map(x => x -> Set(x)).toMap)

  override def is: Fragments =
    "constructing all assignments to a set of variables" ^
      (singletonScheme4v.allAssignmentsTo(Set(0)) === Set(Map(0->0),Map(0->1))) ^
      (singletonScheme4v.allAssignmentsTo(Set(0,1)) === Set(Map(0->0,1->0),Map(0->0,1->1),Map(0->1,1->0),Map(0->1,1->1))) ^
    p^
    "calculating sub-conditions" ^
      (singletonScheme4v.subConditionsOf(Map(0->0), Set(0,1)) === Set(Map(0->0,1->0), Map(0->0, 1->1))) ^
    "allowed values under condition" ^
      (singletonScheme4v.allowedValuesUnderCondition(0,Map(0->0)) === Set(0)) ^
      (singletonScheme4v.allowedValuesUnderCondition(0,Map(1->0)) === Set(0,1)) ^
      (singletonScheme4v.allowedValuesUnderCondition(0,Map(0->1,1->0)) === Set(1)) ^
    p^
    "maximum graphical distance constructor" ^
      "condition only the variable itself" !
        (FactoredScheme.withMaxDistance(Set(0),0,grid(2,2)).conditionersOf(Set(0)) === Set(0)) ^
      "with distance 1" !
        (FactoredScheme.withMaxDistance(Set(0),1,grid(2,2)).conditionersOf(Set(0)) === Set(0,1,2)) ^
      "two conditioners" !
        (FactoredScheme.withMaxDistance(Set(0,2),1,grid(3,1)).conditionersOf(Set(2)) === Set(1,2)) ^
    p^
    "maximum loop length constructor" ^
      "zero loop length" !
        (FactoredScheme.withAllLoopsOfLength(Set(0),0,grid(2,2)).conditionersOf(Set(0)) === Set(0)) ^
      "loops of length one" !
        (FactoredScheme.withAllLoopsOfLength(Set(0),1,grid(2,2)).conditionersOf(Set(0)) === Set(0,1,2,3))
}
