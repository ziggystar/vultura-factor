package vultura.factor

import org.scalacheck.Prop
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import vultura.factor.inference.{MarginalI, ParFunI, JunctionTree}

/**
 * Matchers for use with FastFactor objects.
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
trait FastFactorMatchers {
  def haveSameStructureAs(ref: Factor): Matcher[Factor] = new Matcher[Factor]{
    def apply[S <: Factor](t: Expectable[S]): MatchResult[S] = result(
      ref.variables.deep == t.value.variables.deep && ref.values.size == t.value.values.size,
      "has same structure as " + ref,
      "differs in structure from " + ref,
      t
    )
  }
  def haveValuesCloseTo(ref: Factor, tol: Double = 1e-7): Matcher[Factor] = new Matcher[Factor]{
    def apply[S <: Factor](t: Expectable[S]): MatchResult[S] = result(
      Factor.maxDiff(t.value,ref,NormalD) < tol,
      "has close marginals to " + ref,
      "differs in some value by  " + Factor.maxDiff(t.value,ref,NormalD),
      t
    )
  }

  def beSimilarTo(ref: Factor, tol: Double = 1e-7): Matcher[Factor] =
    haveSameStructureAs(ref) and haveValuesCloseTo(ref,tol)

  def haveExactZ(tol: Double = 1e-7): Matcher[ParFunI] = new Matcher[ParFunI]{
    def apply[S <: ParFunI](t: Expectable[S]): MatchResult[S] = {
      val obtainedZ: Double = t.value.Z
      val exactZ: Double = new JunctionTree(t.value.problem).Z
      result(
        math.abs(obtainedZ - exactZ) < tol,
        "has same Z as exact inference",
        s"has different Z compared to exact inference (got $obtainedZ, exact is $exactZ)",
        t
      )
    }
  }

  def haveExactMarginals(tol: Double = 1e-9) = new Matcher[MarginalI]{
    def apply[S <: MarginalI](t: Expectable[S]): MatchResult[S] = {
      val p = t.value.problem
      val jt = new JunctionTree(p)
      val error: Option[(Int, Double)] = p.variables.map(v =>
        v -> (jt.variableBelief(v).values zip t.value.variableBelief(v).values)
          .map{case (x,y) => math.abs(x-y)}
          .max
      ).find(_._2 > tol)
      result(
        error.isEmpty,
        "has exact marginals",
        s"differs in marginals by ${error.get._2} for variable ${error.get._1}",
        t
      )
    }
  }

  def beCloseTo(ref: Seq[Double], tol: Double = 1e-12): Matcher[Seq[Double]] = new Matcher[Seq[Double]]{
    override def apply[S <: Seq[Double]](t: Expectable[S]): MatchResult[S] = result(
      t.value.zip(ref).map{case (x,y) => math.abs(x - y)}.max < tol,
      "has close values to " + ref,
      "differs in some values by up to " + t.value.zip(ref).map{case (x,y) => math.abs(x - y)}.max,
      t
    )
  }

}
