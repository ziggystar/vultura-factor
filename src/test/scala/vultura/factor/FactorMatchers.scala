package vultura.factor

import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import vultura.factor.inference.{MarginalI, ParFunI, JunctionTree}

/**
 * Matchers for use with FastFactor objects.
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
trait FactorMatchers {
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
      s"$t has close marginals to " + ref,
      s"$t differs in some value by  " + Factor.maxDiff(t.value,ref,NormalD),
      t
    )
  }

  def beSimilarTo(ref: Factor, tol: Double): Matcher[Factor] =
    haveSameStructureAs(ref) and haveValuesCloseTo(ref,tol)

  def haveSameLogZ(inference: Problem => ParFunI, tol: Double): Matcher[ParFunI] = new Matcher[ParFunI]{
    def apply[S <: ParFunI](t: Expectable[S]): MatchResult[S] = {
      val obtainedZ: Double = t.value.logZ
      val otherZ: Double = inference(t.value.problem).logZ
      result(
        math.abs(obtainedZ - otherZ) < tol,
        "has same Z as exact inference",
        s"has different Z compared to exact inference (got $obtainedZ, provided is $otherZ)",
        t
      )
    }
  }
  def haveSameLogZ(other: ParFunI, tol: Double): Matcher[ParFunI] = new Matcher[ParFunI]{
    def apply[S <: ParFunI](t: Expectable[S]): MatchResult[S] = {
      val obtainedZ: Double = t.value.logZ
      val otherZ = other.logZ
      result(
        math.abs(obtainedZ - otherZ) < tol,
        "has same Z as exact inference",
        s"has different Z compared to exact inference (got $obtainedZ, provided is $otherZ)",
        t
      )
    }
  }


  def haveExactMarginals(tol: Double = 1e-9) = haveSameMarginals(new JunctionTree(_),tol)
  def haveExactZ(tol: Double = 1e-9) = haveSameLogZ(new JunctionTree(_),tol)

  def haveSameMarginals(inference: Problem => MarginalI, tol: Double): Matcher[MarginalI] = new Matcher[MarginalI]{
    def apply[S <: MarginalI](t: Expectable[S]): MatchResult[S] = {
      val p = t.value.problem
      val other = inference(p)
      val error: Option[(Int, Double)] = p.variables.map(v =>
        v -> (other.variableBelief(v).values zip t.value.variableBelief(v).values)
          .map{case (x,y) => math.abs(x-y)}
          .max
      ).find(_._2 > tol)
      result(
        error.isEmpty,
        s"$t has exact marginals",
        s"${t.description} differs in marginals by ${error.get._2} for variable ${error.get._1}",
        t
      )
    }
  }

  def haveSameMarginals(other: MarginalI, tol: Double): Matcher[MarginalI] = new Matcher[MarginalI]{
    def apply[S <: MarginalI](t: Expectable[S]): MatchResult[S] = {
      val p = t.value.problem
      val error: Option[(Int, Double)] = p.variables.map(v =>
        v -> (other.variableBelief(v).values zip t.value.variableBelief(v).values)
          .map{case (x,y) => math.abs(x-y)}
          .max
      ).find(_._2 > tol)
      result(
        error.isEmpty,
        "has exact marginals",
        s"${t.description} differs in marginals by ${error.get._2} for variable ${error.get._1}",
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