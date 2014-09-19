package vultura.factor.inference.conditioned

import vultura.factor.Problem
import vultura.factor.inference.MargParI

/** Performs unit propagation using BP result. */
object SlightlyLessSimpleConditioner extends Conditioner {
  def name = "SlightlyLessSimpleConditioner"

  /** extracts a possibly empty condition from a 1-variable factor. */
  override def conditionSimplify(p: Problem, c: GCondition, upMarginals: Option[MargParI] = None): Problem = {
    val upCondition: Option[GCondition] = upMarginals
      .map(up => p.variables.map(v => extractCondition(up.variableBelief(v), p)))
      .map(gcondConjunction(_:_*))
    val theCondition = upCondition.map(gcondConjunction(_,c)).getOrElse(c)
    val (soft,hard) = extractHardConditions(theCondition, p)
    val conditionFactors = gCondToFactors(soft, p)
    val conditioned = p.condition(hard)
    conditioned.copy(factors = conditioned.factors ++ conditionFactors).simplify
  }
}
