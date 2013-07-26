package vultura.factors

import scalaz._
import scalaz.Scalaz._
import vultura.{factors => vf}
import util.Random
import vultura.util.{Measure, TreeWidth}
import collection.immutable.Stream

/**
 * @author Thomas Geier
 * @since 05.02.12
 */

class ProductFactor[T,R](_factors: Seq[T],
                         val productMonoid: Monoid[R])
                        (implicit val fev: Factor[T,R], val cm: ClassManifest[R]) {
  val factors = _factors.toIndexedSeq
  val variables: Array[Int] = factors.flatMap(f => vf.variables(f)).toSeq.distinct.toArray
  val domains: Array[Array[Int]] = variables.map(factors.flatMap(f => vf.variables(f).zip(vf.domains(f))).toMap)
  val variableMaps: IndexedSeq[Array[Int]] = {
    val lookup = variables.zipWithIndex.toMap
    factors.map(f => vf.variables(f).map(lookup))
  }

  def evaluate(assignment: Array[Int]): R = {
    var result = productMonoid.zero
    var i = 0
    while(i < factors.size){
      val f = factors(i)
      val vmap: Array[Int] = variableMaps(i)
      val ass: Array[Int] = {
        val result = new Array[Int](vmap.size)
        var i = 0
        while(i < result.length){
          result(i) = assignment(vmap(i))
          i += 1
        }
        result
      }
      val evalResult = vf.evaluate(f,ass)
      result = productMonoid.append(result,evalResult)
      i += 1
    }
    result
  }

  def conditionView(variables: Array[Int], value: Array[Int]): ProductFactor[FactorView[T],R] = ProductFactor(factors.map(new FactorView(variables zip value toMap, _)),productMonoid)

  def filter(p: T => Boolean): ProductFactor[T, R] = ProductFactor(factors.filter(p),productMonoid)

  /** @return Partition function and `numSamples` samples or `None` if partition is zero. */
  def partitionAndSample(random: Random,
                         measure: Measure[R],
                         numSamples: Int)(implicit cm: ClassManifest[R],
                                          conditionableT: Conditionable[T]): Option[(R,IndexedSeq[Array[Int]])] = {
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

    //last elimination clique should have singleton domain; just evaluate it
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
          val conditioned: ProductFactor[Either[T, TableFactor[R]], R] = condition(eliminationClique,sampleVariables,sampleValues)
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

  def jtPartition(sumMonoid: Monoid[R])(implicit cm: ClassManifest[R]): R =
    upwardCalibratedTrees(sumMonoid).map(_._2).foldLeft(productMonoid.zero)(productMonoid.append(_,_))

  /**
   * Each clique potential inside the trees contains the influence from its children but not from its parent.
   *
   * @return A list of upwardly calibrated junction trees together with their partition function. */
  def upwardCalibratedTrees(sumMonoid: Monoid[R]): Seq[(Tree[TableFactor[R]],R)] = {
    import scalaz.Tree._
    /** marginalize everything except the stuff in variablesOfInterest.
     * @return First is the calibrated tree, second is the upward message from the root.
     */
    def jtMarginalizeUnless(jt: Tree[(Set[Int],Seq[T])], variablesOfInterest: Set[Int] = Set()): (Tree[TableFactor[R]],TableFactor[R]) = {
      val Node((variables, cliques), children) = jt
      val procChildren: Stream[(Tree[TableFactor[R]], TableFactor[R])] = children.map(jtMarginalizeUnless(_,variables))
      val localProduct: ProductFactor[Either[T, TableFactor[R]],R] =
        ProductFactor(cliques.map(Left(_)) ++ procChildren.map(c => Right(c._2)),productMonoid)

      val localFactor = TableFactor.fromFactor(localProduct)
      val upwardMessage = marginalizeAllDense(localFactor,variables -- variablesOfInterest,sumMonoid)
      (node(localFactor,procChildren.map(_._1)),upwardMessage)
    }

    junctionTrees.map(t => jtMarginalizeUnless(t) :-> (_.evaluate(Array())))
  }

  lazy val junctionTrees: Seq[Tree[(Set[Int], Seq[T])]] =
    TreeWidth.compactJTrees(TreeWidth.minDegreeJTs(this.factors.map(f => (vf.variables(f).toSet, f))))

  def minDegreeTreewidth: Int = TreeWidth.minDegreeOrderingAndWidth(this.factors.map(vf.variables(_).toSet))._2

  def addFactor(factor: T) = ProductFactor(factors :+ factor, productMonoid)

  def marginalizeOutVariables(variables : Set[Int],
                              sumMonoid : Monoid[R])(implicit cm : ClassManifest[R],
                                                     tfIsT : =:=[vultura.factors.TableFactor[R], T]): vultura.factors.ProductFactor[T, R] = {
    if(variables.isEmpty)
      this
    else {
      val variable = variables.minBy(v => factors.count(f => vf.variables(f).contains(v)))
      val (affected,unaffected) = factors.partition(f => vf.variables(f).contains(variable))
      val newFactor = vf.marginalizeAllDense(ProductFactor(affected,productMonoid),Set(variable),sumMonoid)
      ProductFactor(unaffected :+ tfIsT(newFactor),productMonoid).marginalizeOutVariables(variables - variable, sumMonoid)
    }
  }
}

object ProductFactor {
  def apply[T,R](_factors: Seq[T], productMonoid: Monoid[R])(implicit fev: Factor[T,R], cm: ClassManifest[R]) =
    new ProductFactor(_factors, productMonoid)

  implicit def pfAsFactor[T,R]: Factor[ProductFactor[T,R],R] = new DenseFactor[ProductFactor[T,R],R] {
    def variables(f: ProductFactor[T, R]): Array[Int] =
      f.variables
    def domains(f: ProductFactor[T, R]): Array[Array[Int]] =
      f.domains
    def evaluate(f: ProductFactor[T, R], assignment: Array[Int]): R =
      f.evaluate(assignment)
    override def partition(f: ProductFactor[T, R], sumMonoid: Monoid[R])(implicit cm: ClassManifest[R]): R =
      f.jtPartition(sumMonoid)
  }

  implicit def pfConditionable[T: Conditionable : ({type F[S] = Factor[S,_]})#F,R]: Conditionable[ProductFactor[T,R]] =
    new Conditionable[ProductFactor[T,R]]{
      def condition(f: ProductFactor[T, R], variables: Array[Int], values: Array[Int]): ProductFactor[T, R] = {
        val zipped: Map[Int, Int] = (variables zip values).toMap
        val reducedFactors = for(
          factor <- f.factors;
          factorVars = vf.variables(factor)
        ) yield {
          //find the assignments from zipped that appear in factorVars and use them for conditioning
          val resultIndices = new Array[Int](factorVars.size)
          var i = 0
          var numHits = 0
          while(i < resultIndices.size){
            if(zipped.contains(factorVars(i))){
              resultIndices(numHits) = i
              numHits += 1
            }
            i += 1
          }

          //copy them to arrays
          val condVars = new Array[Int](numHits)
          val condDoms = new Array[Int](numHits)
          var i2 = 0
          while(i2 < numHits){
            condVars(i2) = factorVars(resultIndices(i2))
            condDoms(i2) = zipped(condVars(i2))
            i2 += 1
          }

          vf.condition(factor,condVars,condDoms)
        }
        val (constant,varying) = reducedFactors.partition(vf.variables(_).size == 0)
        val constantNotZero = constant.filterNot(factor => vf.evaluate(factor,Array()) == f.productMonoid.zero)
        ProductFactor(varying.toIndexedSeq ++ constantNotZero,f.productMonoid)(f.fev, f.cm)
      }
    }
}