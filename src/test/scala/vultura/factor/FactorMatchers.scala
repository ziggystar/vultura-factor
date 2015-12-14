package vultura.factor

import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import vultura.factor.inference.{VariableElimination, MarginalI, ParFunI, JunctionTree}

/**
 * Matchers for use with FastFactor objects.
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
trait FactorMatchers {
  def haveSameStructureAs(ref: Factor): Matcher[Factor] = new Matcher[Factor]{
    def apply[S <: Factor](t: Expectable[S]): MatchResult[S] = result(
      ref.variables.deep == t.value.variables.deep && ref.values.size == t.value.values.size,
      s"${t.description} has same structure as " + ref,
      s"${t.description} differs in structure from " + ref,
      t
    )
  }
  def haveValuesCloseTo(ref: Factor, tol: Double = 1e-7): Matcher[Factor] = new Matcher[Factor]{
    def apply[S <: Factor](t: Expectable[S]): MatchResult[S] = result(
      Factor.maxDiff(t.value,ref,NormalD) < tol,
      s"${t.description} has close marginals to " + ref,
      s"${t.description} differs in some value by  " + Factor.maxDiff(t.value,ref,NormalD),
      t
    )
  }

  def beSimilarTo(ref: Factor, tol: Double): Matcher[Factor] =
    haveSameStructureAs(ref) and haveValuesCloseTo(ref,tol)

  def haveSameLogZ(reference: ParFunI, tol: Double): Matcher[ParFunI] = new Matcher[ParFunI]{
    def apply[S <: ParFunI](t: Expectable[S]): MatchResult[S] = {
      val obtainedZ: Double = t.value.logZ
      val otherZ: Double = reference.logZ
      result(
        math.abs(obtainedZ - otherZ) < tol,
        s"${t.description} has same Z as exact inference",
        f"${t.description} has different Z as expected: ${t.description}: $obtainedZ, expected: $otherZ, diff: ${math.abs(obtainedZ - otherZ)}})",
        t
      )
    }
  }

  def haveExactMarginals(p: Problem, tol: Double = 1e-9) = haveSameMarginals(new JunctionTree(p),tol)
  def haveExactZ(p: Problem, tol: Double = 1e-9) = haveSameLogZ(new JunctionTree(p),tol)

  def haveSameMarginals(reference: MarginalI, tol: Double, logDomain: Boolean = true): Matcher[MarginalI] = new Matcher[MarginalI]{
    def apply[S <: MarginalI](t: Expectable[S]): MatchResult[S] = {
      val p = t.value.problem
      def marg(mi: MarginalI,v: Int): Factor = if(logDomain) mi.logVariableBelief(v) else mi.decodedVariableBelief(v)

      val error: Option[(Int, Double)] = p.variables.map(v =>
        v -> (marg(reference,v).values zip marg(t.value,v).values)
          .map{case (x,y) => math.abs(x-y)}
          .max
      ).find(_._2 > tol)

      val domainString: String = if(logDomain) "log" else "normal"

      result(
        error.isEmpty,
        s"$t has exact marginals",
        s"${t.description} differs in marginals ($domainString encoded) by ${error.get._2} for variable ${error.get._1}",
        t
      )
    }
  }

  def haveValidMarginals: Matcher[MarginalI] = {
    import org.specs2.matcher.Matchers._
    val sumsToOne: Matcher[Iterable[Double]] = beCloseTo(1d,delta=1e-9).^^((_:Iterable[Double]).sum).updateMessage("does not sum to one: " + _)
    val nonNegative: Matcher[Iterable[Double]] = foreach(beGreaterThanOrEqualTo(0d).updateMessage("is negative: " + _))
    val allAreDistributions: Matcher[Iterable[Iterable[Double]]] = foreach(sumsToOne and nonNegative).updateMessage("is not a valid distribution: " + _)
    allAreDistributions ^^ ((margs: MarginalI) => margs.problem.variables.map(margs.varBelief(_).values.toIterable))
  }

  def beCloseToSeq(ref: Seq[Double], tol: Double = 1e-12): Matcher[Seq[Double]] = new Matcher[Seq[Double]]{
    override def apply[S <: Seq[Double]](t: Expectable[S]): MatchResult[S] = result(
      t.value.zip(ref).map{case (x,y) => math.abs(x - y)}.max < tol,
      "has close values to " + ref,
      "differs in some values by up to " + t.value.zip(ref).map{case (x,y) => math.abs(x - y)}.max,
      t
    )
  }
}