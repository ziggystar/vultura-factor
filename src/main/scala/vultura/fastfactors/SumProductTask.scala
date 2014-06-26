package vultura.fastfactors

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
                          ring: RingZ[Double]){
  val numFactors: Int = factorVariables.size
  val remainSize: Int = FastFactor.mapMultiply(remainingVars,domainSizes)

  //collect all variables
  val (cliqueOrdering: Array[Int], margVars: Array[Int]) = {
    val allVars: Array[Int] = factorVariables.flatten.toSet.toArray
    //reorder, so that all margVars are at beginning
    val mv: Array[Int] = allVars.filterNot(remainingVars.contains)
    (mv ++ remainingVars,mv)
  }

  val lookups: Array[Array[Int]] = factorVariables.map(FastFactor.buildLookup(cliqueOrdering,domainSizes,_))(collection.breakOut)

  val margSize: Int = FastFactor.mapMultiply(margVars,domainSizes)

  //domain sizes ordered by the appearance of variables in the clique ordering (for efficient access when counting)
  val cliqueDomains: Array[Int] = cliqueOrdering.map(domainSizes)

  val margTemp: Array[Double] = new Array[Double](margSize)
  val prodTemp: Array[Double] = new Array[Double](numFactors)
  val counter: Array[Int] = new Array[Int](cliqueOrdering.size)
  val factorPointers: Array[Int] = new Array[Int](numFactors)

  final def sumProduct(factorValues: IndexedSeq[Array[Double]], result: Array[Double]) {
    var remainIdx = 0
    var cnt = 0
    while(remainIdx < remainSize){
      var margIdx = 0
      while(margIdx < margSize){
        //increment counter
        val overflow = increment(cnt, counter)
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

  @inline
  private final def increment(count: Int, reg: Array[Int]): Int = {
    var overflow = 0
    val size: Int = reg.length
    while(overflow < size){
      reg(overflow) += 1
      if(reg(overflow) == cliqueDomains(overflow)){
        reg(overflow) = 0
        overflow += 1
      } else {
        return overflow
      }
    }
    overflow
  }
}
