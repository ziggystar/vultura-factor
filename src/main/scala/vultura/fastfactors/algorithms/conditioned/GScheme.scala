package vultura.fastfactors.algorithms.conditioned

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
case class GScheme(_lSchemes: Map[Int,LScheme] = Map()){
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

  def jointScheme(vars: Iterable[Int]) = vars.toSeq.map(lSchemes).foldLeft(LScheme.empty)(_.multiply(_))

  /** @param conditions
    * @param given
    * @return Computes the difference-conditions conditional on condition `given` for each variable. There is no entry for a variable,
    *   if the difference vector is empty. */
  def conditionalContributions(conditions: IndexedSeq[Condition], given: Condition, v: Int): IndexedSeq[Set[Condition]] = {
    val vScheme = lSchemes(v).condition(given)
    val localConditions: IndexedSeq[Set[Map[Int, Int]]] = conditions.map(c => vScheme.condition(c).partialAssignments.toSet)
    val commonSubset = localConditions.reduce(_ intersect _)
    localConditions.map(_ -- commonSubset)
  }
}
