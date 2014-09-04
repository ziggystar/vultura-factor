package vultura.factor.inference.conditioned.lcbp

import vultura.factor.Problem
import vultura.factor.inference.conditioned._

/**
 * A scheme describes the way in which a particular problem is locally conditioned.
 * It is guaranteed, that the conditions C_sup of a set of variables X are sub-conditions
 * of exactly one condition from the set of conditions C_sub of a super-set of variables Y (X \subseteq Y).
 * The conditions factor over single variables, i.e. every global condition can be enforced by a product of 
 * (deterministic 1/0)single variable factors.
 */
trait Scheme {
  /** Values of this type are associated with global conditions. */
  type GC = Condition
  /** This represents the empty condition that enforces no constraints on the assignments. */
  def nullCondition: GC = Map()
  def isSubCondition(c1: GC, c2: GC): Boolean = c1.forall{case (variable, value) => c2.get(variable).contains(value)}
  
  def problem: Problem
  def conditionersOf(variables: Set[Int]): Set[GC]

  /** Compute the condition for `vars` that is super-condition to `c`. */
  def superConditionOf(c: GC, vars: Set[Int]): GC
  def subConditionsOf(c :GC, vars: Set[Int]): Set[GC]
  
  def allowedValuesUnderCondition(variable: Int, condition: GC): Set[Int]
  
  def isVariableEffectedByCondition(variable: Int, condition: GC): Boolean = 
    allowedValuesUnderCondition(variable, condition).size == problem.domains(variable) 
}

/** In a factored scheme, the conditioners of sets of variables are the union of the conditioners of the single variables. */ 
trait FactoredScheme extends Scheme {
  def conditionVariablesOf(variable: Int): Set[Int]
  
  def allAssignmentsTo(variables: Set[Int]): Set[GC] =
    variables.foldLeft(Set(Map(): GC)){case (acc, v) =>
      for{c <- acc; value <- 0 until problem.domains(v)} yield c + (v -> value)
    }
  
  final def conditionersOf(variables: Set[Int]): Set[GC] =
    allAssignmentsTo(variables.map(conditionVariablesOf).reduce(_ ++ _))
}
