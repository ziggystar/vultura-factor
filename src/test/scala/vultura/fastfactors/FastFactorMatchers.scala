package vultura.fastfactors

import org.specs2.matcher.{MatchResult, Expectable, Matcher}

/**
 * Matchers for use with FastFactor objects.
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
trait FastFactorMatchers {
  def haveSameStructureAs(ref: FastFactor): Matcher[FastFactor] = new Matcher[FastFactor]{
    def apply[S <: FastFactor](t: Expectable[S]): MatchResult[S] = result(
      ref.variables.deep == t.value.variables.deep && ref.values.size == t.value.values.size,
      "has same structure as " + ref,
      "differs in structure from " + ref,
      t
    )
  }
  def haveValuesCloseTo(ref: FastFactor, tol: Double = 1e-7): Matcher[FastFactor] = new Matcher[FastFactor]{
    def apply[S <: FastFactor](t: Expectable[S]): MatchResult[S] = result(
      FastFactor.maxDiff(t.value,ref,NormalD) < tol,
      "has close marginals to " + ref,
      "differs in some value by  " + FastFactor.maxDiff(t.value,ref,NormalD),
      t
    )
  }
  def beSimilarTo(ref: FastFactor, tol: Double = 1e-7): Matcher[FastFactor] =
    haveSameStructureAs(ref) and haveValuesCloseTo(ref,tol)
}
