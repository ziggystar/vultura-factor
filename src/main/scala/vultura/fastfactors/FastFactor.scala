package vultura.fastfactors

import scala.reflect.ClassTag

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 5/31/13
 */
case class FastFactor(variables: Array[Int], values: Array[Double]){
  require(FastFactor.isStrictlyIncreasing(variables), "variables are not ordered increasingly")
}

object FastFactor{
  def orderIfNecessary(variables: Array[Int], values: Array[Double], domains: Array[Int]) = {
    val ordered = variables.sorted
    val newValues = new Array[Double](values.size)
    sumProduct(ordered,domains,Array(variables),Array(values),SafeD,newValues)
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
    sumProduct(variables,domains,factors.map(_.variables),factors.map(_.values),ring,values)
    FastFactor(variables,values)
  }

  def multiplyMarginalize(ring: RingZ[Double])(domains: Array[Int])(factors: Seq[FastFactor], marginalize: Array[Int]): FastFactor = {
      val variables = merge(factors.map(_.variables),exclude=marginalize)
      val numValues = variables.map(domains).foldLeft(1)(_ * _)
      val values = new Array[Double](numValues)
      sumProduct(variables,domains,factors.map(_.variables)(collection.breakOut),factors.map(_.values)(collection.breakOut),ring,values)
      FastFactor(variables,values)
    }
  /**
   *
   * @param varOrdering Variable ordering used for counting in column-major ordering.
   * @param domainSizes
   * @param factorVariables
   * @return An array `r`, holding the index-increment into the factors data array, when the
   *         counter over the variables in `varOrdering` overflows at position `i`. The pointer `p` into
   *         the factor array has to be adjusted by `p = p + r(i)`.
   */
  def buildLookup(varOrdering: Array[Int], domainSizes: Array[Int], factorVariables: Array[Int]): Array[Int] = {
    val strides = factorVariables.map(domainSizes).scanLeft(1)(_ * _)
    //now we walk over the counting variables
    //for each earlier variable, we subtract d_i * stride_i (overflow);
    //for the current variable we add stride_i (increment)
    //mod will only hold the negative overflow values
    var mod = 0
    var i = 0
    var result = new Array[Int](varOrdering.size + 1)
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
  def incrementCounter(reg: Array[Int], domains: Array[Int]): Int = {
    reg(0) += 1
    if(reg(0) != domains(0))
      return 0
    var overflow = 0
    do {
      reg(overflow) = 0
      overflow += 1
      if(overflow < reg.size)
        reg(overflow) += 1
    } while(overflow < reg.size && reg(overflow) == domains(overflow))
    overflow
  }

  def sumProduct[@specialized(Double) T: ClassTag](remainingVars: Array[Int],
                                                   domainSizes: Array[Int],
                                                   factorVariables: IndexedSeq[Array[Int]],
                                                   factorValues: IndexedSeq[Array[T]],
                                                   ring: RingZ[T],
                                                   result: Array[T]) {
    val numFactors: Int = factorValues.size
    require(factorVariables.size == numFactors)

    //collect all variables
    val cliqueOrdering = {
      val allVars = factorVariables.flatten.toSet.toArray
      assert(remainingVars.forall(allVars.contains), "trying to marginalize out non-existent variable")
      //reorder, so that all margVars are at beginning
      allVars.filterNot(remainingVars.contains) ++ remainingVars
    }
    assert(result.size == remainingVars.map(domainSizes).product, "result array must fit exactly")

    val lookups: Array[Array[Int]] = factorVariables.map(buildLookup(cliqueOrdering,domainSizes,_))(collection.breakOut)

    val remainSize: Int = remainingVars.map(domainSizes).product
    val margSize: Int = cliqueOrdering.drop(remainingVars.size).map(domainSizes).product

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
        val overflow = incrementCounter(counter,cliqueDomains)

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
}
