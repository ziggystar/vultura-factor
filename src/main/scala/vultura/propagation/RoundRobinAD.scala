package vultura.propagation

import gnu.trove.map.hash.TIntObjectHashMap
import vultura.util.SIIndex

import scala.collection.mutable

/** Simple calibrator that traverses the nodes in the same order until convergence. */
class RoundRobinAD(cp: CP[ADImpl],
                   differ: Differ[ADImpl#NodeType],
                   initializer: IValuation[ADImpl#NodeType]){
  type N = ADImpl#NodeType
  private val nodesIndex: SIIndex[N] = new SIIndex(cp.nodes)
  private val nodes = nodesIndex.elements
  val implementations = nodes.map(n => cp.implementationOf(n).orNull)
  val descendants: IndexedSeq[IndexedSeq[N]] = nodes.map(cp.descendantsOf)
  private val state: Array[Array[Double]] = nodes.map(_.construct)(collection.breakOut)
  val stateSizes: Array[Int] = state.map(_.length)
  //to retrieve the temporal result array for node at position `i`, look at `tempStorage(tempStorageIndex(i))`
  //this could be optimized by placing the indices into tempStorage into an array (replacing stateSizes)
  private val tempStorage: TIntObjectHashMap[Array[Double]] = {
    val lookup = new TIntObjectHashMap[Array[Double]](20)
    stateSizes.distinct.foreach{ l =>
      lookup.put(l,new Array[Double](l))
    }
    lookup
  }
  /** Those nodes that have to be provided with an initial value. */
  def inputNodes: IndexedSeq[N] = nodes.filterNot(n => cp.implementationOf(n).isDefined)

  def calibrate(params: IValuation[N],
                maxDiff: Double = 1e-9,
                maxLoops: Long = 1000,
                specificInitializer: IValuation[N] = IValuation.empty): Calibrated[N] = {
    val isValid: mutable.BitSet = new mutable.BitSet(nodes.size)
    //initialize the nodes
    nodes.zip(state).zipWithIndex.foreach{case ((n,ad),i) =>
        if(cp.implementationOf(n).isDefined) {
          specificInitializer.orElse(initializer).istore(n,ad)
          isValid(i) = false
        } else {
          params.istore(n,ad)
          isValid(i) = true
        }
    }

    //run the calibration loop
    var converged = false
    var loops = 0
    while(loops < maxLoops && !converged) {
      loops += 1
      converged = true

      var i = 0
      while (i < nodes.size) {
        if (!isValid(i)) {
          val node = nodes(i)
          val impl = implementations(i)
          val stateSize: Int = stateSizes(i)
          val newResult = tempStorage.get(stateSize)
          //NPE at this line means we have an invalid parameter node here, which means initialization is broken
          impl.apply(cp.dependenciesOf(node).get.map(n => state(nodesIndex(n)))(collection.breakOut), newResult)
          val diff = differ.diff(node, state(i), newResult)
          if(diff > maxDiff){
            converged = false
            //update value by swapping
            tempStorage.put(stateSize, state(i))
            state(i) = newResult
            //invalidate descendants
            for(desc <- descendants(i)){
              isValid(nodesIndex(desc)) = false
            }
          }
          isValid(i) = true
        }
        i += 1
      }
    }
    new Calibrated[N] {
      override def totalUpdates: Long = loops
      override def isConverged: Boolean = converged
      override def ival: IValuation[N] = new IValuation[N] {
        val myState = state.clone()
        override def isDefinedAt(n: N): Boolean = nodesIndex.contains(n)
        override def istore(n: N, r: N#TImpl): Unit = {
          val v: Array[Double] = myState(nodesIndex(n))
          System.arraycopy(v,0,r,0,v.length)
        }
      }
    }
  }
}

trait Calibrated[N <: Node]{
  def totalUpdates: Long
  def isConverged: Boolean
  def ival: IValuation[N]
}

