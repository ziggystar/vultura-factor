package vultura.fastfactors

import scala.reflect.ClassTag
import vultura.util.IntDomainCPI

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 5/31/13
 */
case class FastFactor(variables: Array[Int], values: Array[Double]){
  require(FastFactor.isStrictlyIncreasing(variables), "variables are not ordered increasingly")

  /** Check whether this factor is independent of some variable. (with no tolerance!) */
  def simplify(domains: Array[Int]): FastFactor = {
    def isIndependentIn(factor: FastFactor, variable: Int): Boolean = {
      val remVars: Array[Int] = factor.variables.filterNot(_ == variable)
      val resultArray = new Array[Double](remVars.foldLeft(1)(_ * domains(_)))
      FastFactor.sumProduct(remVars,domains,Array(factor.variables),Array(factor.values): Array[Array[Double]],FastFactor.AdditionIsEquality,resultArray)
      resultArray.forall(d => !d.isNaN)
    }
    this.variables.foldLeft(this){case (factor, variable) =>
      if(isIndependentIn(factor,variable)) {
        //println("removing var " + variable + " in factor " + factor)
        factor.condition(Map(variable -> 0),domains)
      } else
        factor
    }
  }

  def condition(condition: Map[Int,Int], domains: Array[Int]) = {
    val (hitVars,remVars) = this.variables.partition(condition.contains)
    val remDomains = remVars.map(domains)
    val strides: Array[Int] = this.variables.map(domains).scanLeft(1)(_ * _)

    //maps variables to their stride
    val strideLookup: Map[Int, Int] = this.variables.zip(strides.init).toMap
    //we'll use the overflow position of the counter to index into this array
    val lookup: Array[Int] = FastFactor.buildLookup(remVars,domains,this.variables)
    var pos: Int = hitVars.map(v => strideLookup(v) * condition(v)).sum

    //this holds result
    val condVals: Array[Double] = new Array[Double](remVars.map(domains).product)

    //loop state
    val countReg: Array[Int] = Array.fill(remVars.size)(0)
    var i = 0

    //work begins here
    condVals(i) = this.values(pos)
    while(i  < condVals.size - 1){
      i += 1
      //side-effecting call
      val overflow = FastFactor.incrementCounter(countReg,remDomains)
      pos += lookup(overflow)
      condVals(i) = this.values(pos)
    }

    FastFactor(remVars,condVals)
  }

  def normalize(ring: RingZ[Double]) = FastFactor(variables,ring.normalize(values))

  override def equals(obj: Any): Boolean = obj match {
    case ff: FastFactor => ff.eq(this) || (ff.variables.sameElements(variables) && ff.values.sameElements(values))
    case _ => false
  }

  override lazy val hashCode: Int = (variables.toSeq,values.toSeq).hashCode()

  override def toString: String = f"FastFactor(VAR: ${variables.mkString(",")},VAL: ${values.mkString(",")})"
  def toBriefString: String = f"${variables.mkString(",")} | ${values.map("%.2f".format(_)).mkString(",")}"
  def map(f: Double => Double): FastFactor = this.copy(values = values.map(f))

  def index(values: Array[Val], domains: Array[Int]): Int =
    values.zip(variables.map(domains)).foldLeft((0,1)){
    case ((acc,stride),(value,domainSize)) => (acc + value * stride, stride * domainSize)}._1

  def eval(vals: Array[Val], domains: Array[Int]): Double = values(index(vals,domains))
  def set(vals: Array[Val], domains: Array[Int], to: Double): FastFactor =
    copy(values = {
      val newArray: Array[Double] = this.values.clone()
      newArray(index(vals,domains)) = to
      newArray
    })
}

object FastFactor{
  implicit class RichRing(val ring: RingZ[Double]) extends AnyVal {
    def encode(f: FastFactor): FastFactor = f.copy(values=ring.encode(f.values))
    def decode(f: FastFactor): FastFactor = f.copy(values=ring.decode(f.values))
  }

  /** Sums the given factors element-wise. It's the ordinary sum of Double values. And all scopes have to be equal. */
  def elementWiseSum(factors: IndexedSeq[FastFactor]): FastFactor = {
    require(!factors.isEmpty, "cannot be called with empty argument")
    val scope = factors.head.variables
    require(factors.forall(_.variables.sameElements(scope)), "all scopes must be equal")
    FastFactor(scope,Array.tabulate(factors.head.values.size)(i => factors.foldLeft(0d)(_ + _.values(i))))
  }

  object AdditionIsEquality extends RingZ[Double]{
    def zero: Double = sys.error("operation not supported")

    def one: Double = sys.error("operation not supported")

    def sum(s1: Double, s2: Double): Double = if(s1 == s2) s1 else Double.NaN

    def prod(f1: Double, f2: Double): Double = sys.error("operation not supported")

    override def sumA(ss: Array[Double]): Double = {
      val x = ss(0)
      var i = 1
      while(i < ss.length){
        if(ss(i) != x)
          return Double.NaN
        i += 1
      }
      x
    }

    override def prodA(fs: Array[Double]): Double = {
      require(fs.length == 1)
      fs(0)
    }

    implicit def tag: ClassTag[Double] = implicitly[ClassTag[Double]]
  }

  /**
   * Builds a factor, adhering to a partial assignment to variables, spreading the remaining probability mass
   * evenly across all allowed assignments.
   * @param deterministic Adhere to these deterministic variable assignments.
   * @return a `FastFactor` with a max entropy distribution over the given variables.
   */
  def deterministicMaxEntropy(variables: Array[Int], deterministic: Map[Int,Int], domains: Array[Int], ring: RingZ[Double]): FastFactor =
    fromFunction(
      variables,
      domains,
      assign => if(assign.zip(variables).exists{case (value,variable) => deterministic.get(variable).exists(_ != value)}) ring.zero else ring.one
    ).normalize(ring)


  def maxEntropy(variables: Array[Int], domains: Array[Int], ring: RingZ[Double]): FastFactor =
    FastFactor(variables, ring.normalize(Array.fill(variables.foldLeft(1)(_ * domains(_)))(ring.one))).normalize(ring)

  def fromFunction(variables: Array[Int], domains: Array[Int], f: Array[Int] => Double): FastFactor =
    FastFactor(variables, new IntDomainCPI(variables.map(v => Array.range(0,domains(v)))).map(f)(collection.breakOut))

  def orderIfNecessary(variables: Array[Int], values: Array[Double], domains: Array[Int]): FastFactor = {
    val ordered = variables.sorted
    val newValues = new Array[Double](values.size)
    sumProduct(ordered,domains,Array(variables),Array(values): Array[Array[Double]],SafeD,newValues)
    FastFactor(ordered,newValues)
  }

  def isStrictlyIncreasing(xs: Array[Int]): Boolean = {
    var last = Integer.MIN_VALUE
    var i = 0
    while(i < xs.size){
      if(xs(i) <= last) return false
      last = xs(i)
      i += 1
    }
    true
  }

  /** Merge some sorted sequences of integers into a new array. */
  def merge(xxs: Seq[Array[Int]], exclude: Array[Int] = Array()): Array[Int] = {
    xxs.flatten.distinct.sorted.filterNot(exclude.contains).toArray
  }

  def multiply(ring: RingZ[Double])(domains: Array[Int])(factors: IndexedSeq[FastFactor]): FastFactor = {
    val variables = merge(factors.map(_.variables))
    val numValues = variables.map(domains).foldLeft(1)(_ * _)
    val values = new Array[Double](numValues)
    sumProduct(variables,domains,factors.map(_.variables)(collection.breakOut),factors.map(_.values)(collection.breakOut): Array[Array[Double]],ring,values)
    FastFactor(variables,values)
  }

  /**
   *
   * @param ring
   * @param domains
   * @param factors
   * @param marginalize These variables will be summed out.
   * @return
   */
  def multiplyMarginalize(ring: RingZ[Double])(domains: Array[Int])(factors: Seq[FastFactor], marginalize: Array[Int]): FastFactor = {
    val variables = merge(factors.map(_.variables),exclude=marginalize)
    val numValues = mapMultiply(variables,domains)
    val values = new Array[Double](numValues)
    sumProduct(variables,domains,factors.map(_.variables)(collection.breakOut),factors.map(_.values)(collection.breakOut): Array[Array[Double]],ring,values)
    FastFactor(variables,values)
  }

  def multiplyRetain(ring: RingZ[Double])(domains: Array[Int])(factors: Seq[FastFactor], retain: Array[Int]): FastFactor = {
    val numValues = mapMultiply(retain,domains)
    val values = new Array[Double](numValues)
    sumProduct(retain,domains,factors.map(_.variables)(collection.breakOut),factors.map(_.values)(collection.breakOut): Array[Array[Double]],ring,values)
    FastFactor(retain,values)
  }

  /**
   *
   * @param varOrdering Variable ordering used for counting in column-major ordering.
   * @param domainSizes domainSizes[i] is domain size of variable x_i.
   * @param factorVariables These are the variables of the factor, in the according order.
   * @return An array `r`, holding the index-increment into the factors data array, when the
   *         counter over the variables in `varOrdering` overflows at position `i`. The pointer `p` into
   *         the factor array has to be adjusted by `p = p + r(i)`.
   */
  def buildLookup(varOrdering: Array[Int], domainSizes: Array[Int], factorVariables: Array[Int]): Array[Int] = {
    val strides: Array[Int]=  {
      val r = new Array[Int](factorVariables.length)
      var prod = 1
      var i = 0
      while(i < r.length){
        r(i) = prod
        prod *= domainSizes(factorVariables(i))
        i += 1
      }
      r
    }
    //now we walk over the counting variables
    //for each earlier variable, we subtract d_i * stride_i (overflow);
    //for the current variable we add stride_i (increment)
    //mod will only hold the negative overflow values
    var mod = 0
    var i = 0
    val result = new Array[Int](varOrdering.size + 1)
    while(i < varOrdering.size){
      val v = varOrdering(i)
      val factorIndex = factorVariables.indexOf(v)

      if(factorIndex != -1){
        result(i) = mod + strides(factorIndex)
        mod -= strides(factorIndex) * (domainSizes(v) - 1)
      } else {
        result(i) = mod
      }
      i += 1
    }
    result(i) = mod

    result
  }

  /** Exception occurs when counter overflows.
    * @return The figure after which the overflow occurred. 0 means no overflow. */

  @inline
  final def incrementCounter(reg: Array[Int], domains: Array[Int]): Int = {
    reg(0) += 1
    if(reg(0) != domains(0))
      return 0
    var overflow = 0
    val size: Int = reg.length
    do {
      reg(overflow) = 0
      overflow += 1
      if(overflow < size)
        reg(overflow) += 1
    } while(overflow < size && reg(overflow) == domains(overflow))
    overflow
  }

  /** Does same as `incrementCounter`, but works for empty `reg` arrays. Is a bit slower.
    * @see incrementCounter
    */
  @inline
  final def incrementCounter2(reg: Array[Int], domains: Array[Int]): Int = {
    var overflow = 0
    val size: Int = reg.length
    while(overflow < size){
      reg(overflow) += 1
      if(reg(overflow) == domains(overflow)){
        reg(overflow) = 0
        overflow += 1
      } else {
        return overflow
      }
    }
    overflow
  }

  /**
   *
   * @param remainingVars The resulting factor will range over these variables in the given order.
   * @param domainSizes
   * @param factorVariables
   * @param factorValues
   * @param ring
   * @param result
   */
  def sumProduct[@specialized(Double) T: ClassTag](remainingVars: Array[Int],
                                                   domainSizes: Array[Int],
                                                   factorVariables: Array[Array[Int]],
                                                   factorValues: Array[Array[T]],
                                                   ring: RingZ[T],
                                                   result: Array[T]) {
    val numFactors: Int = factorValues.size
    require(factorVariables.size == numFactors)
    val remainSize: Int = mapMultiply(remainingVars,domainSizes)
    require(result.size == remainSize, "result array must fit exactly")

    //collect all variables
    val (cliqueOrdering,margVars) = {
      val allVars: Array[Int] = factorVariables.flatten.toSet.toArray
      //reorder, so that all margVars are at beginning
      val mv: Array[Int] = allVars.filterNot(remainingVars.contains)
      (mv ++ remainingVars,mv)
    }

    val lookups: Array[Array[Int]] = factorVariables.map(buildLookup(cliqueOrdering,domainSizes,_))(collection.breakOut)

    val margSize: Int = mapMultiply(margVars,domainSizes)

    //will hold intermediate results, which will get summed over
    val margTemp: Array[T] = new Array[T](margSize)
    val prodTemp: Array[T] = new Array[T](numFactors)

    //domain sizes ordered by the appearance of variables in the clique ordering (for efficient access when counting)
    val cliqueDomains: Array[Int] = cliqueOrdering.map(domainSizes)

    //counting register
    val counter: Array[Int] = new Array[Int](cliqueOrdering.size)

    //factor pointers
    val factorPointers: Array[Int] = new Array[Int](numFactors)

    var remainIdx = 0
    while(remainIdx < remainSize){

      var margIdx = 0
      while(margIdx < margSize){

        //increment counter
        val overflow = incrementCounter2(counter,cliqueDomains)

        //calculate the factor contributions
        //NOTE: we collect the factor values for the counter state before its update in this loop!
        var fi = 0
        while(fi < numFactors){
          prodTemp(fi) = factorValues(fi)(factorPointers(fi))
          factorPointers(fi) += lookups(fi)(overflow)
          fi += 1
        }

        //multiply factor values
        margTemp(margIdx) = ring.prodA(prodTemp)
        margIdx += 1
      }
      //now sum over marginalized variables for one assignment to the remaining variables
      result(remainIdx) = ring.sumA(margTemp)
      remainIdx += 1
    }
  }



  /**
   * @param is Array of indices into `factors`.
   * @return $\Prod_{i \in is} factors(i)
   */
  @inline
  final def mapMultiply(is: Array[Int],factors: Array[Int]): Int = {
    var product = 1
    var i = 0
    while(i < is.length){
      product *= factors(is(i))
      i += 1
    }
    product
  }

  /** Calculate KL divergence for two factors, given in normal representation. */
  def kl(f1: FastFactor, f2: FastFactor, ring: RingZ[Double]): Double = {
    require(ring == NormalD)
    require(f1.values.length == f2.values.length)
    var result = 0d
    var i = 0
    while(i < f1.values.length){
      result += f1.values(i) * math.log(f1.values(i) / f2.values(i))
      i += 1
    }
    result
  }
  /** Max diff for two factors, given in normal representation. */
  def maxDiff(f1: FastFactor, f2: FastFactor, ring: RingZ[Double]): Double = {
    require(ring == NormalD, "only use this in normal domain (for no good reason)")
    require(f1.values.length == f2.values.length, "comparing factors of unequal length")
    var result = 0d
    var i = 0
    while(i < f1.values.length){
      result = math.max(result,math.abs(f1.values(i) - f2.values(i)))
      i += 1
    }
    result
  }
}