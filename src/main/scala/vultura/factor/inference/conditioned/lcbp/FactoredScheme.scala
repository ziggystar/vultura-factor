package vultura.factor.inference.conditioned.lcbp

import vultura.factor.{Var, Problem}

/** In a factored scheme, the conditioners of sets of variables are the union of the conditioners of the
  * single variables. Thus a `FactoredScheme` is defined by the mapping of each single variable to a
  * set of conditioning variables. */
case class FactoredScheme(problem: Problem, conditionRelations: Map[Var,Set[Var]] = Map()) extends Scheme {
  val conditioners: Map[Var, Set[Var]] = conditionRelations.withDefaultValue(Set.empty)
  def allConditioners: Set[Int] = problem.variables.flatMap(conditioners)

  def conditionersOf(vs: Set[Int]): Set[Int] = vs.map(conditioners).reduce(_ ++_)

  def allAssignmentsTo(variables: Set[Int]): Set[GC] =
    variables.foldLeft(Set(Map(): GC)){case (acc, v) =>
      for{c <- acc; value <- 0 until problem.domains(v)} yield c + (v -> value)
    }

  /** Compute the condition for `vars` that is super-condition to `c`.
    * This limits the conditioned variables in `c` to the conditioners of `vars`. */
  override def superConditionOf(c: GC, vars: Set[Int]): GC = {
    val conds = conditionersOf(vars)
    c.filter(kv => conds(kv._1))
  }

  override def subConditionsOf(c: GC, vars: Set[Int]): Set[GC] =
    allAssignmentsTo(conditionersOf(vars) -- c.keySet).map(c ++ _)

  /** This method is required to enforce the condition within the mixture components. */
  override def allowedValuesUnderCondition(variable: Int, condition: GC): Set[Int] =
    condition.get(variable).map(Set(_)).getOrElse((0 until problem.domains(variable)).toSet)

  final def conditionsOf(variables: Set[Int]): Set[GC] = allAssignmentsTo(conditionersOf(variables))
}
