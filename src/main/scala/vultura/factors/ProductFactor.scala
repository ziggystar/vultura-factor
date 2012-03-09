package vultura.factors

import scalaz._
import scalaz.Scalaz._
import vultura.{factors => vf}
import util.Random
import vultura.util.{Measure, TreeWidth}
import collection.immutable.List

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

  def partitionAndSample(random: Random, sumMonoid: Monoid[R])(implicit cm: ClassManifest[R], measure: Measure[R]): (R,Array[Int]) = {
    if(this.factors.isEmpty)
      return (this.productMonoid.zero, Array())

    //work along this variable ordering; currently obtained by min degree heuristic
    val ordering = TreeWidth.minDegreeOrdering(this.factors.toSeq.map(vf.variables(_).toSet)).map(this.variables zip this.domains map(t => t._1 -> t) toMap)

    //obtain a sequence of factors, retaining the elimination factor
    val leftFactors: Seq[Either[T, TableFactor[R]]] = this.factors.toSeq.map(Left(_))

    //create the sequence of elimination cliques
    val eliminationSeries: List[(Seq[Either[T, TableFactor[R]]], ProductFactor[Either[T, TableFactor[R]], R])] = ordering.scanLeft(
      (leftFactors, null: ProductFactor[Either[T, TableFactor[R]], R])
    ){
        case ((remainingFactors,_), (eliminationVariable,domain)) =>
          val (participating, agnostic) = remainingFactors.partition(factor => vf.variables(factor).contains(eliminationVariable))
          val eliminationFactor: ProductFactor[Either[T, TableFactor[R]], R] = ProductFactor(participating,this.productMonoid)
          val marginalizedEliminationFactor: TableFactor[R] = vf.marginalizeDense(eliminationFactor,Array(eliminationVariable),Array(domain),sumMonoid)
          (agnostic :+ Right(marginalizedEliminationFactor),eliminationFactor)
      }

    //last elimination clique should have singlular domain; just evaluate it
    val partition = vf.evaluate(ProductFactor(eliminationSeries.last._1,this.productMonoid),Array())

    //now sample by using the elimination cliques in reverse order
    val rElimSeries: List[ProductFactor[Either[T, TableFactor[R]], R]] = eliminationSeries.tail.reverse.map(_._2)
    //as input, we need the sample so far (as a list of (variable,value) tuples) to condition the next elimination clique
    val sample = rElimSeries.foldLeft((Array[Int](),Array[Int]())){case ((sampleVariables,sampleValues), eliminationClique) =>
      //condtition on sample so far
      val conditioned: ProductFactor[Either[T, TableFactor[R]], R] = eliminationClique.condition(sampleVariables,sampleValues)
      val extensionVariables = vf.variables(conditioned)
      val extensionValues = vf.sample(conditioned, random)
      (sampleVariables ++ extensionVariables, sampleValues ++ extensionValues)
    }

    //now sort the sample to the order of this factor
    val sortedSampleValues = this.variables.map(sample.zipped.toMap)

    (partition, sortedSampleValues)
  }
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
  }
}