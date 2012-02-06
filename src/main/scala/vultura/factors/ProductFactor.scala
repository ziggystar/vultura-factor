package vultura.factors

import scalaz._
import scalaz.Scalaz._
import vultura.{factors => vf}
import collection.Iterable

/**
 * Can only wrap factors of a type that marginalizes to itself, thus `Factor[T,R,T]` for some `T`.
 * @author Thomas Geier
 * @since 05.02.12
 */

case class ProductFactor[T,R](factors: Iterable[Either[T,DenseFactor[R]]],
                              productMonoid: Monoid[R])
                             (implicit fev: Factor[T,R],
                              cmr: ClassManifest[R]) {

  val variables: Array[Int] = factors.flatMap(f => vf.variables(f)).toSeq.distinct.toArray
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
    val (constant,varying: Iterable[Either[T, DenseFactor[R]]]) = reducedFactors.partition(vf.variables(_).size == 0)
    val constantValue = constant.map(vf.evaluate(_, Array())).reduce(productMonoid.append(_,_))
    val singleConstantFactor: DenseFactor[R] = DenseFactor.constantFactor(constantValue)
    ProductFactor(varying.toIndexedSeq :+ Right(singleConstantFactor),productMonoid)
  }
}

object ProductFactor {
  implicit def pfAsFactor[T,R]: Factor[ProductFactor[T,R],R] = new Factor[ProductFactor[T,R],R] {
    def variables(f: ProductFactor[T, R]): Array[Int] =
      f.variables
    def domains(f: ProductFactor[T, R]): Array[Array[Int]] =
      f.domains
    def evaluate(f: ProductFactor[T, R], assignment: Array[Int]): R =
      f.evaluate(assignment)
    def condition(f: ProductFactor[T, R], variables: Array[Int], values: Array[Int]): ProductFactor[T, R] =
      f.condition(variables,values)
  }
}