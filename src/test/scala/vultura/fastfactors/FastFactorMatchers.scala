package vultura.fastfactors

import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import vultura.fastfactors.algorithms.{CalibratedJunctionTree, InfAlg}

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

  def haveExactZ(tol: Double = 1e-7): Matcher[InfAlg] = new Matcher[InfAlg]{
    def apply[S <: InfAlg](t: Expectable[S]): MatchResult[S] = {
      val obtainedZ: Double = t.value.Z
      val exactZ: Double = new CalibratedJunctionTree(t.value.getProblem).Z
      result(
        math.abs(obtainedZ - exactZ) < tol,
        "has same Z as exact inference",
        f"has different Z compared to exact inference (got ${obtainedZ}, exact is ${exactZ})",
        t
      )
    }
  }

}
