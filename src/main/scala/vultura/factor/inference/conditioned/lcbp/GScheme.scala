package vultura.factor.inference.conditioned.lcbp

import vultura.factor.inference.conditioned._

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
sealed trait GScheme {
  /** Create the conditions for a factor with the given scope.
    * @return the conditions created by the product of the conditions of `vars`. */
  def jointConditions(vars: Iterable[Int]): Seq[Condition]

  /** @return The conditions for a given variable. */
  def variableConditions(v: Int): Seq[Condition]

  /** Meant to compute the condition of a variable that is active for a given condition of an adjacent factor. */
  def superCondition(v: Int, c: Condition): Condition

  /** Meant to compute the condition of a variable that is active for a given condition of an adjacent factor. */
  def superConditionJoint(vars: Iterable[Int], c: Condition): Condition

  def subConditions(c: Condition, vars: Iterable[Int]): Seq[Condition]

  /** @param conditions
    * @param given
    * @return Computes the difference-conditions conditional on condition `given` for each variable. There is no entry for a variable,
    *         if the difference vector is empty. */
  def conditionalContributions(conditions: IndexedSeq[Condition], given: Condition, v: Int): IndexedSeq[Set[Condition]]
}

case class LSchemeBasedGScheme(_lSchemes: Map[Int, LScheme] = Map()) extends GScheme {
  val lSchemes = _lSchemes.withDefaultValue(LScheme.empty)

  /** Create the conditions for a factor with the given scope.
    * @return the conditions created by the product of the conditions of `vars`. */
  def jointConditions(vars: Iterable[Int]): Seq[Condition] = jointScheme(vars).partialAssignments

  /** @return The conditions for a given variable. */
  def variableConditions(v: Int): Seq[Condition] = lSchemes(v).partialAssignments

  /** Meant to compute the condition of a variable that is active for a given condition of an adjacent factor. */
  def superCondition(v: Int, c: Condition): Condition = {
    val singleAssignment = lSchemes(v).condition(c).partialAssignments
    assert(singleAssignment.size == 1, "found more than one super condition for a variable")
    singleAssignment.head
  }

  /** Meant to compute the condition of a variable that is active for a given condition of an adjacent factor. */
  def superConditionJoint(vars: Iterable[Int], c: Condition): Condition = {
    val singleAssignment = vars.toSeq.map(lSchemes).foldLeft(LScheme.empty)(_.multiply(_)).condition(c).partialAssignments
    assert(singleAssignment.size == 1, "found more than one super condition for a variable")
    singleAssignment.head
  }

  def subConditions(c: Condition, vars: Iterable[Int]): Seq[Condition] = jointScheme(vars).condition(c).partialAssignments

  def jointScheme(vars: Iterable[Int]): LScheme = vars.toSeq.map(lSchemes).foldLeft(LScheme.empty)(_.multiply(_))

  /** @param conditions
    * @param given
    * @return Computes the difference-conditions conditional on condition `given` for each variable. There is no entry for a variable,
    *         if the difference vector is empty. */
  def conditionalContributions(conditions: IndexedSeq[Condition], given: Condition, v: Int): IndexedSeq[Set[Condition]] = {
    val vScheme: LScheme = lSchemes(v).condition(given)
    val localConditions: IndexedSeq[Set[Map[Int, Int]]] = conditions.map(c => vScheme.condition(c).partialAssignments.toSet)
    val commonSubset = localConditions.reduce(_ intersect _)
    localConditions.map(_ -- commonSubset)
  }
}

case class NaiveGScheme(_splitVariables: Map[Int, Set[Int]] = Map(), domains: Array[Int]) extends GScheme {
  val splitVariables: Map[Int, Set[Int]] = _splitVariables.withDefault(_ => Set())
  /** @param conditions
    * @param given
    * @return Computes the difference-conditions conditional on condition `given` for each variable. There is no entry for a variable,
    *         if the difference vector is empty. */
  override def conditionalContributions(conditions: IndexedSeq[Condition], given: Condition, v: Int): IndexedSeq[Set[Condition]] = ???

  override def subConditions(c: Condition, vars: Iterable[Int]): Seq[Condition] = {
    val relevantVariables: Set[Int] = vars.flatMap(splitVariables).toSet
    assert(c.keySet.subsetOf(relevantVariables))
    enumerateConditions(relevantVariables -- c.keys).map(_ ++ c)
  }

  /** Meant to compute the condition of a variable that is active for a given condition of an adjacent factor. */
  override def superConditionJoint(vars: Iterable[Int], c: Condition): Condition = {
    val relevantVariables: Set[Int] = vars.flatMap(splitVariables).toSet
    assert(relevantVariables.subsetOf(c.keySet))
    c.filterKeys(relevantVariables)
  }

  /** Meant to compute the condition of a variable that is active for a given condition of an adjacent factor. */
  override def superCondition(v: Int, c: Condition): Condition = {
    assert(splitVariables(v).subsetOf(c.keySet))
    c.filterKeys(splitVariables(v))
  }

  /** @return The conditions for a given variable. */
  override def variableConditions(v: Int): Seq[Condition] = jointConditions(Seq(v))

  /** Create the conditions for a factor with the given scope.
    * @return the conditions created by the product of the conditions of `vars`. */
  override def jointConditions(vars: Iterable[Int]): Seq[Condition] = enumerateConditions(vars.flatMap(splitVariables).toSet)

  /** enumerate all possible conditions for a set of variables */
  def enumerateConditions(variables: Set[Int]): Seq[Condition] = {
    if (variables.isEmpty) Seq(Map())
    else for (currentAssignment <- 0 until domains(variables.head); tailAssignment <- enumerateConditions(variables.tail)) yield tailAssignment + (variables.head -> currentAssignment)
  }
}

object GScheme {
  def apply(domains: Array[Int]): GScheme = NaiveGScheme(Map(), domains)

  def apply(domains: Array[Int], lSchemes: Map[Int, LScheme]): GScheme = NaiveGScheme(lSchemes.mapValues(_.usedVariables), domains)

  def apply(splitVariables: Map[Int, Set[Int]], domains: Array[Int]): GScheme = NaiveGScheme(splitVariables, domains)
}