package vultura.factors

import scalaz._
import scalaz.Scalaz._
import vultura.{factors => vf}

/**
 * @author Thomas Geier
 * @since 05.02.12
 */

case class ProductFactor[T,R](factors: Iterable[T],
                              productMonoid: Monoid[R])
                             (implicit val fev: Factor[T,R]) {

  val variables: Array[Int] = factors.flatMap(f => vf.variables(f)).toSeq.distinct.toArray
  val domains: Array[Array[Int]] = variables.map(factors.flatMap(f => vf.variables(f).zip(vf.domains(f))).toMap)

  def evaluate(assignment: Array[Int]): R = {
    val lookup = variables.zip(assignment).toMap
    val results = for(
      f <- factors;
      ass = vf.variables(f).map(lookup)
    ) yield vf.evaluate(f,ass)
    results.foldLeft(productMonoid.zero)(productMonoid.append(_,_))
  }

  def condition(variables: Array[Int], value: Array[Int]): ProductFactor[T,R] = {
    val zipped = variables zip value
    val reducedFactors = for(
      f <- factors;
      fv = vf.variables(f);
      //take only
      (fvars,fvals) = zipped.filter(t => fv.contains(t._1)).unzip
    ) yield vf.condition(f,fvars.toArray,fvals.toArray)
    val (constant,varying) = reducedFactors.partition(vf.variables(_).size == 0)
    val constantNotZero = constant.filterNot(f => vf.evaluate(f,Array()) == productMonoid.zero)
    ProductFactor(varying.toIndexedSeq ++ constantNotZero,productMonoid)
  }

  def filter(p: T => Boolean): ProductFactor[T, R] = ProductFactor(factors.filter(p),productMonoid)
}

object ProductFactor {
  implicit def pfAsFactor[T,R]: Factor[ProductFactor[T,R],R] = new DenseFactor[ProductFactor[T,R],R] {
    def variables(f: ProductFactor[T, R]): Array[Int] =
      f.variables
    def domains(f: ProductFactor[T, R]): Array[Array[Int]] =
      f.domains
    def evaluate(f: ProductFactor[T, R], assignment: Array[Int]): R =
      f.evaluate(assignment)
    def condition(f: ProductFactor[T, R], variables: Array[Int], values: Array[Int]): ProductFactor[T, R] =
      f.condition(variables,values)
    //TODO more efficient partition and sample if there are completely independent subsets of factors
    override def partition(f: ProductFactor[T, R], sumMonoid: Monoid[R])(implicit cm: ClassManifest[R]): R = {
      if(f.factors.isEmpty) return f.productMonoid.zero
      implicit val factorEvidence: Factor[T, R] = f.fev
      val ordering = f.variables zip f.domains
      //obtain a sequence of factors, retaining the elimination factor
      val leftFactors: Seq[Either[T, TableFactor[R]]] = f.factors.toSeq.map(Left(_))
      //this should hold the final elimination clique and all constant factors
      val eliminationSeries: Seq[Either[T, TableFactor[R]]] = ordering.foldLeft(leftFactors){
          case (remainingFactors, (eliminationVariable,domain)) =>
            val (participating, agnostic) = remainingFactors.partition(factor => vf.variables(factor).contains(eliminationVariable))
            val eliminationFactor = ProductFactor(participating,f.productMonoid)
            val marginalizedEliminationFactor: TableFactor[R] = vf.marginalizeDense(eliminationFactor,Array(eliminationVariable),Array(domain),sumMonoid)
            agnostic :+ Right(marginalizedEliminationFactor)
        }
      val result = vf.evaluate(ProductFactor(eliminationSeries,f.productMonoid),Array())
      result
    }
  }
}