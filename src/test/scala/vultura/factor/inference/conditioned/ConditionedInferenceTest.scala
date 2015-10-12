package vultura.factor.inference.conditioned

import org.specs2.mutable.Specification
import vultura.factor.{SampleProblems, Problem}

class ConditionedInferenceTest extends Specification {

  "Conditioned inference should not crash on smokers 10 (bug)" >> {
    val problem = SampleProblems.getExample("smokers_10.uai").problem
    new ConditionedInference(problem, runInitially = 10)() must throwA[Exception].not
  }

}
