package vultura.factors

import scalaz._
import scalaz.Scalaz._
import vultura.{factors => vf}
import util.Random
import vultura.util.{Measure, TreeWidth}

/**
 * @author Thomas Geier
 * @since 05.02.12
 */

case class ProductFactor[T,R](_factors: Seq[T],
                              productMonoid: Monoid[R])
                             (implicit val fev: Factor[T,R]) {
  val factors = _factors.toIndexedSeq
  val variables: Array[Int] = factors.flatMap(f => vf.variables(f)).toSeq.distinct.toArray
  val domains: Array[Array[Int]] = variables.map(factors.flatMap(f => vf.variables(f).zip(vf.domains(f))).toMap)

  def evaluate(assignment: Array[Int]): R = {
    val lookup = variables.zip(assignment).toMap
    var result = productMonoid.zero
    var i = 0
    while(i < factors.size){
      val f = factors(i)
      val assignment = vf.variables(f).map(lookup)
      val evalResult = vf.evaluate(f,assignment)
      result = productMonoid.append(result,evalResult)
      i += 1
    }
    result
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

  def conditionView(variables: Array[Int], value: Array[Int]): ProductFactor[FactorView[T],R] = ProductFactor(factors.map(new FactorView(variables zip value toMap, _)),productMonoid)

  def filter(p: T => Boolean): ProductFactor[T, R] = ProductFactor(factors.filter(p),productMonoid)

  /** @return Partition function and `numSamples` samples or `None` if partition is zero. */
  def partitionAndSample(random: Random, measure: Measure[R], numSamples: Int)(implicit cm: ClassManifest[R]): Option[(R,IndexedSeq[Array[Int]])] = {
    val sumMonoid = measure.sum
    if(this.factors.isEmpty)
      return Some((this.productMonoid.zero, IndexedSeq.fill(numSamples)(Array.empty[Int])))

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
    assert(!partition.asInstanceOf[Double].isPosInfinity, "infinite partition function")

    if(!measure.isPositive(partition))
      return None

    //now sample by using the elimination cliques in reverse order
    lazy val rElimSeries: List[ProductFactor[Either[T, TableFactor[R]], R]] = eliminationSeries.tail.reverse.map(_._2)
    //as input, we need the sample so far (as a list of (variable,value) tuples) to condition the next elimination clique
    //sample holds a sequence of variables and their assignment
    def sample: Array[Int] = {
      //the sample is produced in the elimination ordering
      val unsortedSample = rElimSeries.foldLeft((Array[Int](),Array[Int]()): (Array[Int],Array[Int])){
        case ((sampleVariables,sampleValues), eliminationClique) => {
          //condition on sample so far; this could lead to factors becoming constant and thus variables becoming independent
          //thus we need to track these variables and sample them in an additional step
          val priorVariablesAndDomains: Array[(Int, Array[Int])] = eliminationClique.variables zip eliminationClique.domains
          val conditioned: ProductFactor[Either[T, TableFactor[R]], R] = eliminationClique.condition(sampleVariables,sampleValues)
          val extensionVariables = vf.variables(conditioned)

          val extensionValues: Array[Int] = if(extensionVariables.size > 0)
            vf.sample(conditioned, random, measure).get //we can get here, since we made sure the partition function is not zero
          else
            Array[Int]()

          import vultura.util._
          val (indiVars, indiVals) = priorVariablesAndDomains.filterNot(vd => extensionVariables.contains(vd._1) || sampleVariables.contains(vd._1)).map{
            case (variable,domain) => (variable,domain.toIndexedSeq.pickRandom(random))
          }.unzip
          (indiVars.toArray ++ sampleVariables ++ extensionVariables, indiVals.toArray ++ sampleValues ++ extensionValues)
        }
      }
      val sampleAsMap: Map[Int, Int] = unsortedSample.zipped.toMap
      this.variables.map(sampleAsMap)
    }

    Some((partition, IndexedSeq.fill(numSamples)(sample)))
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