package vultura.factor.inference.conditioned.lcbp

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor.{NormalD, Problem}

class FactoredSchemeTest extends Specification {
  val problem4v = Problem(IndexedSeq(), Array(2,2,2,2), NormalD)
  val singletonScheme4v = FactoredScheme(problem4v, (0 to 3).map(x => x -> Set(x)).toMap)
  override def is: Fragments =
    "constructing all assignments to a set of variables" ^
      (singletonScheme4v.allAssignmentsTo(Set(0)) === Set(Map(0->0),Map(0->1))) ^
      (singletonScheme4v.allAssignmentsTo(Set(0,1)) === Set(Map(0->0,1->0),Map(0->0,1->1),Map(0->1,1->0),Map(0->1,1->1))) ^
    p^
    "calculating subconditions" ^
      (singletonScheme4v.subConditionsOf(Map(0->0), Set(0,1)) === Set(Map(0->0,1->0), Map(0->0, 1->1))) ^
    "allowed values under condition" ^
      (singletonScheme4v.allowedValuesUnderCondition(0,Map(0->0)) === Set(0)) ^
      (singletonScheme4v.allowedValuesUnderCondition(0,Map(1->0)) === Set(0,1)) ^
      (singletonScheme4v.allowedValuesUnderCondition(0,Map(0->1,1->0)) === Set(1))


}
