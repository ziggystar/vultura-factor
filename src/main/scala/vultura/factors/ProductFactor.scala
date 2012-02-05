package vultura.factors

import scalaz.Monoid
import vultura.{factors => vf}

/**
 * Can only wrap factors of a type that marginalizes to itself, thus `Factor[T,R,T]` for some `T`.
 * @author Thomas Geier
 * @since 05.02.12
 */

case class ProductFactor[T,R: ClassManifest](factors: Iterable[Either[T,DenseFactor[R]]], monoid: Monoid[R])(implicit fev: Factor[T,R,T]) extends SelfFactor[R,ProductFactor[T,R]] {
  val variables: Array[Int] = factors.flatMap(f => vf.variables(f)).toSeq.distinct.toArray
  val domains: Array[Array[Int]] = variables.map(factors.flatMap(f => vf.variables(f).zip(vf.domains(f))).toMap)

  def evaluate(assignment: Array[Int]): R = {
    val lookup = variables.zip(assignment).toMap
    val results = for(
      f <- factors;
      ass = vf.variables(f).map(lookup)
    ) yield vf.evaluate(f,ass)
    results.reduce(monoid.append(_,_))
  }

  def condition(variables: Array[Int], value: Array[Int]): ProductFactor[T,R] = {
    val zipped = variables zip value
    val reducedFactors = for(
      f <- factors;
      fv = vf.variables(f);
      //take only
      (fvars,fvals) = zipped.filter(t => fv.contains(t._1)).unzip
    ) yield vf.condition(f,fvars.toArray,fvals.toArray)
    ProductFactor(reducedFactors,monoid)(fev)
  }

  /** This "stupid" marginalize collects all dependent factors and marginalizes them into one DenseFactor. */
  def marginalize(variables: Array[Int], domains: Array[Array[Int]])(implicit mm: Monoid[R]): ProductFactor[T,R] = {
    //mm(+) and monoid(*) must distribute for the following to work; this is currently unchecked

    val varSet = variables.toSet
    val (dependentFactors,independentFactors) = factors.partition(vf.variables(_).exists(varSet))

    sys.error("continue here")
  }
}