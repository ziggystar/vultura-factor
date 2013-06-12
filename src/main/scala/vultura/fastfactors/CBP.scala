package vultura.fastfactors

import scala.util.Random
import scala.collection.immutable.TreeSet

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/12/13
 */
class CBP(factors: IndexedSeq[FastFactor],
          domains: Array[Int],
          ring: RingZ[Double],
          random: Random = new Random,
          leafSelection: BeliefPropagation => Double,
          variableSelection: BeliefPropagation => Int,
          bpMaxiter: Int = 1000,
          bpTol: Double = 1e-7) {
  implicit val queueOrdering = new Ordering[(Double,Map[Int,Int],BeliefPropagation)]{
    def compare(x: (Double, Map[Int, Int], BeliefPropagation), y: (Double, Map[Int, Int], BeliefPropagation)): Int =
      if(x._1 < y._1) -1 else if(x == y) 0 else +1
  }
  var queue: TreeSet[(Double,Map[Int,Int],BeliefPropagation)] = _
  var iterations: Int = _

  init()

  def init(){
    queue = TreeSet(constructBP(Map()))
    iterations = 0
  }

  def run(maxIter: Int, maxH: Double){
    while(iterations < maxIter && queue.head._1 > maxH){
      val (_,selectAssignment,selectLeafBP) =  queue.head
      val selectVar: Int = variableSelection(selectLeafBP)
      val newAssignments = for(x <- 0 until domains(selectVar)) yield (selectAssignment + (selectVar -> x))
      queue = queue.tail ++ newAssignments.map(constructBP)
    }
  }

  def constructBP(assignment: Map[Int,Int]): (Double,Map[Int,Int],BeliefPropagation) = {
    val bp = new BeliefPropagation(factors,domains,ring,random)
    bp.run(bpMaxiter,bpTol)
    (leafSelection(bp),assignment,bp)
  }
  def logZ: Double = ???
  def variableBelief(vi: Int): FastFactor = ???
}
