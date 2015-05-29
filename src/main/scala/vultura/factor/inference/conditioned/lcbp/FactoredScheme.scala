package vultura.factor.inference.conditioned.lcbp

import vultura.factor.{Var, Problem}

import scala.annotation.tailrec

/** In a factored scheme, the conditioners of sets of variables are the union of the conditioners of the
  * single variables. Thus a `FactoredScheme` is defined by the mapping of each single variable to a
  * set of conditioning variables. */
case class FactoredScheme(problem: Problem, conditionRelations: Map[Var,Set[Var]] = Map()) extends Scheme {
  val conditioners: Map[Var, Set[Var]] = conditionRelations.withDefaultValue(Set.empty)
  def allConditioners: Set[Int] = problem.variableSet.flatMap(conditioners)

  def conditionersOf(vs: Set[Int]): Set[Int] = vs.map(conditioners).foldLeft(Set[Int]())(_ ++_)

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

  def toGScheme: GScheme = GScheme(conditionRelations,problem.domains)
}

object FactoredScheme{
  /** Construct a factored scheme from a map that maps a conditioner to all its conditionees. */
  def fromInfluenceMap(problem: Problem, influenceMap: Map[Int,Set[Int]]): FactoredScheme =
    FactoredScheme(problem, influenceMap.toSeq.flatMap{case (conditioner,conditionees) => conditionees.map(_ -> conditioner)}.groupBy(_._1).map{case(conditionee,tuples) => conditionee -> tuples.map(_._2).toSet})
  /** Constructs a [[vultura.factor.inference.conditioned.lcbp.FactoredScheme]] that conditions on the
    * given variables, and assigns all nodes with a maximal graphical distance to the sets of conditionees.
    * @param variables The conditioners.
    * @param maxDistance Add all variables with this maximum distance from conditioner to set of conditionees.
    */
  def withMaxDistance(variables: Set[Int], maxDistance: Int, problem: Problem): FactoredScheme = {
    @tailrec
    def collect(from: Set[Int], d: Int = maxDistance): Set[Int] =
      if(d <= 0) from
      else collect(from ++ from.flatMap(vi => problem.neighboursOfVariableEx(vi)), d - 1)
    fromInfluenceMap(problem,variables.map(v => v -> collect(Set(v)))(collection.breakOut))
  }

  /** Condition the problem completely on the given variables. This yields a scheme that corresponds
    * to a balanced and statically ordered CBP tree.
    */
  def fullyConditioned(variables: Set[Int], problem: Problem): FactoredScheme =
    fromInfluenceMap(problem,variables.map(_ -> problem.variableSet)(collection.breakOut))
}
