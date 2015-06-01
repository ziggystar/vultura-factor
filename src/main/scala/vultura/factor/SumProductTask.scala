package vultura.factor

/** An instance of this class represents a sum-product operation on some factors with. The only thing that may change
  * are the values of the involved factors. Using this class to represents a sp-operation allows for very fast
  * operation.
  * @see sumProduct
  *
  * @param remainingVars
  * @param domainSizes
  * @param factorVariables
  */
case class SumProductTask(remainingVars: Array[Int],
                          domainSizes: Array[Int],
                          factorVariables: Array[Array[Int]],
                          ring: Ring[Double]){
  val numFactors: Int = factorVariables.length
  val remainSize: Int = Factor.mapMultiply(remainingVars,domainSizes)

  //collect all variables
  val (cliqueOrdering: Array[Int], margVars: Array[Int]) = {
    val allVars: Array[Int] = factorVariables.flatten.distinct
    //reorder, so that all margVars are at beginning
    val mv: Array[Int] = allVars.filterNot(remainingVars.contains)
    (mv ++ remainingVars,mv)
  }
  val lookups: Array[Array[Int]] = factorVariables.map(Factor.buildLookup(cliqueOrdering,domainSizes,_))(collection.breakOut)
  val margSize: Int = Factor.mapMultiply(margVars,domainSizes)
  //domain sizes ordered by the appearance of variables in the clique ordering (for efficient access when counting)
  val cliqueDomains: Array[Int] = cliqueOrdering.map(domainSizes)
  val counterSize: Int = cliqueOrdering.length

  /** Holds the temporary values that get summed, get overwritten before being read in sumProduct. */
  val margTemp: Array[Double] = new Array[Double](margSize)
  /** Holds the temporary values that get multiplied. Get written to before being read in sumProduct. */
  val prodTemp: Array[Double] = new Array[Double](numFactors)
  val counter: Array[Int] = new Array[Int](counterSize)
  val factorPointers: Array[Int] = new Array[Int](numFactors)

  final def sumProduct(factorValues: IndexedSeq[Array[Double]], result: Array[Double]) {
    //TODO maybe the clearing is not needed
    java.util.Arrays.fill(counter,0)
    java.util.Arrays.fill(factorPointers,0)
    var remainIdx = 0
    var cnt = 0
    while(remainIdx < remainSize){
      var margIdx = 0
      while(margIdx < margSize){
        //increment counter
        val overflow = increment(cnt)
        cnt += 1
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

  final def sumProductNormalize(factorValues: IndexedSeq[Array[Double]], result: Array[Double]) {
    //TODO maybe the clearing is not needed
    java.util.Arrays.fill(counter,0)
    java.util.Arrays.fill(factorPointers,0)
    var remainIdx = 0
    var cnt = 0
    while(remainIdx < remainSize){
      var margIdx = 0
      while(margIdx < margSize){
        //increment counter
        val overflow = increment(cnt)
        cnt += 1
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
    ring.normalizeInplace(result)
  }

  /** Increments the counter.
    * Mutates the `counter` member.
    * @param count The current step number of the counter.
    * @return The counter figure where the overflow occurred.
    */
  @inline
  private final def increment(count: Int): Int = {
    var overflow = 0

    while(overflow < counterSize){
      counter(overflow) += 1
      if(counter(overflow) == cliqueDomains(overflow)){
        counter(overflow) = 0
        overflow += 1
      } else {
        return overflow
      }
    }
    overflow
  }
}
