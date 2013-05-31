package vultura.factors

import scalaz._
import Scalaz._
import vultura.util._
import util.Random

/**
 * Stores all function values inside an array.
 * `variables` is guaranteed to be sorted.
 */
class TableFactor[T: ClassManifest](val variables: Array[Int],
                                    val domains: Array[Array[Int]],
                                    val data: Array[T],
                                    val independentVariables: Array[Int] = Array()){
  val cpi = new IntDomainCPI(variables zip domains filterNot (independentVariables contains _._1) map (_._2))

  assert(variables.size == domains.size && cpi.size == data.size, "creating invalid TableFactor")

  def deterministicAssignment(m: Measure[T]): Map[Int,Int] = {
    var numPositives = 0
    var lastPositiveIndex = -1
    var i = 0
    while(i < data.size){
      if(m.isPositive(data(i))) {
        numPositives += 1
        lastPositiveIndex = i
        if(numPositives > 1)
          i = data.size
      }
      i += 1
    }
    if(numPositives == 1)
      variables.zip(cpi.index2Seq(lastPositiveIndex)).toMap
    else
      Map.empty
  }

  def denseData: Array[T] = if (independentVariables.isEmpty) data else new IntDomainCPI(domains).map(evaluate)(collection.breakOut)

  def constantValue: Option[T] = if(independentVariables.size == variables.size) Some(data(0)) else None

  val dependentIndices = {
    val independentIndices = independentVariables map (variables indexOf _)
    (variables indices).toArray filterNot (independentIndices contains)
  }

  def evaluate(assignment: Array[Int]): T = {
    val dependentAssignment: Array[Int] =
      if (independentVariables.isEmpty) assignment
      else dependentIndices map assignment

    data(cpi.seq2Index(dependentAssignment))
  }

  /** Condition via marginalization. */
  def condition(vars: Array[Int],
                values: Array[Int]): TableFactor[T] = {
    implicit val fakeMonoid: Monoid[T] = new Monoid[T]{
      def append(s1: T, s2: => T): T = sys.error("append on monoid on marginalization for conditioning should not be used")
      lazy val zero: T = sys.error("zero on monoid on marginalization for conditioning should not be used")
    }
    TableFactor.marginalizeDense(this, vars, values.map(Array(_)),fakeMonoid)
  }

  def map[S: ClassManifest](f: T => S): TableFactor[S] = new TableFactor(variables, domains, data.map(f), independentVariables)

  def withIndependentVariables(ivs: Array[Int], idoms: Array[Array[Int]]): TableFactor[T] = new TableFactor(
    variables ++ ivs,
    domains ++ idoms,
    data,
    ivs
  )

  lazy val hc = variables.deep.hashCode ^ domains.deep.hashCode ^ data.deep.hashCode ^ independentVariables.deep.hashCode
  override def hashCode(): Int = hc

  override def equals(obj: Any): Boolean = obj match {
    case tf: TableFactor[T] =>
      this.hashCode == tf.hashCode &&
      this.variables.deep == tf.variables.deep &&
      this.domains.deep == tf.domains.deep &&
      this.data.deep == tf.data.deep &&
      this.independentVariables.deep == tf.independentVariables.deep
    case _ => false
  }

  override def toString: String = "TableFactor(%s)".format(variables.mkString(","))

  def normalized(measure: Measure[T]): TableFactor[Double] =
    new TableFactor(variables, domains, measure.normalize(data),independentVariables)
}

object TableFactor {
  import vultura.{factors => vf}

  def fromFunction[T: ClassManifest](_vars: Seq[Int], _domains: Seq[Array[Int]], f: Array[Int] => T) = {
    require(_vars.size == _domains.size, "variable number and domain number don't match")

    val (sortedVars, sortedDomains) = _vars.zip(_domains).unzip

    val cpi = new IntDomainCPI(seqarray2aa(sortedDomains))
    val table = new Array[T](cpi.size)
    var i = 0
    val firstValue: T = f(cpi.head)
    var isConstant = true
    while(i < cpi.size){
      val assign = cpi(i)
      val value: T = f(assign)
      //assert(!value.asInstanceOf[Double].isPosInfinity, "putting infinity into table factor")
      isConstant &= (value == firstValue)
      table(i) = value
      i += 1
    }

    if(isConstant)
      constantFactor(firstValue).withIndependentVariables(sortedVars.toArray, sortedDomains)
    else
      new TableFactor(sortedVars.toArray, sortedDomains.map(_.toArray).toArray, table)
  }

  def fromFactor[T,A](f: A)(implicit ev: Factor[A,T],cm: ClassManifest[T]): TableFactor[T] =
    TableFactor.fromFunction(vf.variables(f),vf.domains(f),vf.evaluate(f,_))

  def fromTable[T: ClassManifest](_vars: Seq[Int], _domains: Seq[Array[Int]], f: IndexedSeq[T]) = {
    import vultura.util._
    val cpi = new IntDomainCPI(_domains)
    this.fromFunction(_vars, _domains, ass => f(cpi.seq2Index(ass.toArray)))
  }

  def constantFactor[T: ClassManifest](singleValue: T) = new TableFactor(Array(),Array.empty[Array[Int]],Array(singleValue))

  implicit def dfAsFactor[R]: DenseFactor[TableFactor[R],R] = new DenseFactor[TableFactor[R],R]{
    def variables(f: TableFactor[R]): Array[Int] = f.variables
    def domains(f: TableFactor[R]): Array[Array[Int]] = f.domains
    def evaluate(f: TableFactor[R], assignment: Array[Int]): R = f.evaluate(assignment)
  }

  implicit def dfConditionable[T: ClassManifest] = new Conditionable[TableFactor[T]]{
    def condition(f: TableFactor[T], variables: Array[Int], values: Array[Int]): TableFactor[T] = f.condition(variables,values)
  }

  /**
   * Generalization of marginalization and conditioning. This method sums over all given domain values
   * for the specified variables. Specifying only one domain value for a variable is equivalent to conditioning
   * on this value.
   * Note: The function must not rely on `monoid` being non-null if no domain contains more than one entry.
   *
   * @param f Object to apply to.
   * @param _vars These variables will be eliminated from the function.
   * @param _doms Zips with `variables`. Contains at least one domain value for each variable in `variables`. If multiple values are given for
   *  a certain variable, the variable is eliminated by summing over the given values.
   * @param monoid Use this for summing the result values of type `T`.
   * @return A `Factor` that does not depend on any variable in `variables`.
   */
  def marginalizeDense[A,B](f: A,
                            _vars: Array[Int],
                            _doms: Array[Array[Int]],
                            monoid: Monoid[B])
                           (implicit evF: Factor[A,B],
                            manifestB: ClassManifest[B]): TableFactor[B] = {
    import vultura.{factors => vf}

    val (margVars, margDoms) = _vars.zip(_doms).unzip

    val MASK = 0x80000000

    val (variables,domains) = (vf.variables(f) zip vf.domains(f)).unzip

    require(margVars.forall(variables.contains), "trying to marginalize a variable that's not there")

    val (remainingVars, remainingDomains) = variables.zip(domains).filterNot(t => margVars.contains(t._1)).unzip

    //this tells us where to look for a value when constructing an assignment to this Fun
    //if high bit is unset remainingVars and else to the summed out vars
    //this is not in imperative style for optimization purposes but for readability :/
    val lookUp: Array[Int] = {
      var intoResult = 0
      val result = new Array[Int](variables.size)
      while (intoResult < variables.size) {
        val margVarIndex = margVars.indexOf(variables(intoResult))
        //got -1 if the variable is not contained in `margVars`
        if(margVarIndex != -1){
          result(intoResult) = margVarIndex | MASK
        } else {
          //we need to find a match here
          val remainVarIndex = remainingVars.indexOf(variables(intoResult))
          result(intoResult) = remainVarIndex
        }
        intoResult += 1
      }

      result
    }

    val reusedAssignment = new Array[Int](variables.size)
    /**`reconstructAssignment` reconstructs an assignment to `f`, given an assignment to remaining variables and
     * to marginalized variables, separately. Both given arrays have to adhere to the order of `margVars` and `remainingVars`.
     * @return `reusedAssignment` is overwritten and returned.
     */
    def reconstructAssignment(remainAss: Array[Int], newAss: Array[Int]): Array[Int] = {
      var i = 0
      while (i < reusedAssignment.size) {
        val index = lookUp(i)
        val value = if ((index & MASK) != 0) newAss(index ^ MASK) else remainAss(index)
        reusedAssignment(i) = value
        i += 1
      }
      reusedAssignment
    }

    val _cpi = new IntDomainCPI(margDoms)

    //fix the assignment to the remaining variables and sum over everything in vars/doms
    //the argument to sumOut goes to the remaining variables
    val sumOut: Array[Int] => B = { assignmentToRemaining =>
      //the cpi should not be empty
      var result = evaluate(f,reconstructAssignment(assignmentToRemaining, _cpi.head))
      var i = 1
      while(i < _cpi.size){
        result = monoid.append(result,evaluate(f,reconstructAssignment(assignmentToRemaining,_cpi(i))))
        i += 1
      }
      result
    }

      TableFactor.fromFunction(remainingVars, remainingDomains, sumOut)
  }
}