package vultura.factors

import scalaz.Monoid
import vultura.{factors => vf}

/**
 * Can only wrap factors of a type that marginalizes to itself, thus `Factor[T,R,T]` for some `T`.
 * @author Thomas Geier
 * @since 05.02.12
 */

case class ProductFactor[T,R](factors: Iterable[Either[T,SelfFactor[R]]],
                              productMonoid: Monoid[R])
                             (implicit fev: Factor[T,R],
                              cmr: ClassManifest[R]) extends SelfFactor[R] {

  val variables: Array[Int] = factors.flatMap(f => vf.variables(f)(eitherFactor(fev,SelfFactor.sf2f[R]))).toSeq.distinct.toArray
  val domains: Array[Array[Int]] = variables.map(factors.flatMap(f => vf.variables(f).zip(vf.domains(f))).toMap)

  def evaluate(assignment: Array[Int]): R = {
    val lookup = variables.zip(assignment).toMap
    val results = for(
      f <- factors;
      ass = vf.variables(f).map(lookup)
    ) yield vf.evaluate(f,ass)
    results.reduce(productMonoid.append(_,_))
  }

  def condition(variables: Array[Int], value: Array[Int]): ProductFactor[T,R] = {
    val zipped = variables zip value
    val reducedFactors = for(
      f <- factors;
      fv = vf.variables(f);
      //take only
      (fvars,fvals) = zipped.filter(t => fv.contains(t._1)).unzip
    ) yield vf.condition(f,fvars.toArray,fvals.toArray)
    ProductFactor(reducedFactors,productMonoid)
  }
}