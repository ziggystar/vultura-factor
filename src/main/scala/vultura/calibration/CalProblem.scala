package vultura.calibration

import vultura.factor.inference.ConvergenceStats
import vultura.util.SIIndex
import vultura.util.graph2.graphviz.DotGraph

trait Edge

/** Features:
  * - damping
  * - mutable updating of edge values (set operation)
  * - type-safe lookup (not necessary if all values are double arrays)
  *
  * What does a problem do?
  *
  *  - define a set of edges
  */
trait CalProblem {
  /** Low level representation of values. Only arrays of doubles. */
  type LRep = Array[Double]

  type E <: Edge

  /** Edge type. */
  trait Edge {
    type D <: E
    /** Size of the array required to store the state of this edge. */
    def arraySize: Int
    def dependencies: IndexedSeq[D]
    /**
     * - first parameter: `zip`s with `dependencies`.
     * - second parameter: Result of computation shall be stored here. Content of result is garbage.
      */
    def compute(ins: Array[LRep], result: LRep): Unit
  }

  /** Constructs a new initial value for each edge. */
  def initializer: E => LRep

  def edges: Set[E]

  def computationGraph: DotGraph[E,(E,E)] =
    DotGraph[E,(E,E)](edges, for (e <- edges; d <- e.dependencies) yield (d, e)).labelNodes{case e => e.toString}
}

trait ResultBuilder[R] {outer: CalProblem =>
  def buildResult(valuation: outer.E => LRep): R
}

/** Mutable class that holds a calibration state. */
class Calibrator[CP <: CalProblem](val cp: CP) {
  type LRep = Array[Double]
  type Edge = cp.E

  /** An edge index. */
  type EI = Int

  protected val edges: SIIndex[Edge] = new SIIndex(cp.edges)
  protected var state: Array[LRep] = edges.elements.map(cp.initializer)(collection.breakOut)
  protected val dependencies: IndexedSeq[IndexedSeq[EI]] = edges.elements.map(_.dependencies.map(edges(_)))

  val stronglyConnectedComponents: IndexedSeq[Set[EI]] = {
    import vultura.util.graph2._
    val graph = ChildList(
      edges.indices.toSet,
      edges.indices.map(ei => ei -> dependencies(ei).toSet)(collection.breakOut): Map[EI, Set[EI]]
    )
    tarjanSCC(graph).toIndexedSeq
  }

  protected def calibrateComponent(componentIndex: Int, maxIterations: Long, maxDiff: Double, damping: Double = 0d): ConvergenceStats = {
    def newEdgeValue(ei: EI): Array[Double] = {
      //update edge
      val edge = edges.backward(ei)
      val newValue = new Array[Double](edge.arraySize)
      edge.compute(dependencies(ei).map(state)(collection.breakOut), newValue)
      newValue
    }

    val component = stronglyConnectedComponents(componentIndex)
    if(component.size == 1) {
      //just update the edge
      val ei = component.head
      state(ei) = newEdgeValue(ei)
      ConvergenceStats(iterations = 1, maxDiff = 0, isConverged = true)
    } else {
      val componentEdges: IndexedSeq[EI] = component.toIndexedSeq
      var iteration = 0L
      var iterationDiff: Double = 0d
      do {
        iterationDiff = 0d

        var cei = 0
        while(cei < componentEdges.size){
          val ei = componentEdges(cei)

          //update edge
          val oldValue = state(ei)
          val newValue = newEdgeValue(ei)

          var newDiff = 0d
          var point = 0
          while(point < newValue.length){
            newDiff = math.max(newDiff,math.abs(newValue(point) - oldValue(point)))
            newValue(point) = (1-damping)*newValue(point) + damping * oldValue(point)
            point += 1
          }
          iterationDiff = math.max(iterationDiff,newDiff)
          state(ei) = newValue
          cei += 1
        }

        iteration += 1
      } while (iterationDiff > maxDiff && iteration < maxIterations)

      ConvergenceStats(iteration,iterationDiff,iterationDiff < maxDiff)
    }
  }

  def calibrate(maxIterations: Long, maxDiff: Double, damping: Double = 0d): ConvergenceStats = {
    require(damping >= 0 && damping < 1d, s"damping has to be within [0,1[, but is $damping")
    stronglyConnectedComponents.indices.map(scc => calibrateComponent(scc,maxIterations,maxDiff,damping)).reduce(_ max _)
  }

  def reset(): Unit = {
    state = edges.elements.map(cp.initializer)(collection.breakOut)
  }

  def edgeState(edge: Edge): LRep = state(edges(edge))
  def updateState(edge: Edge, newValue: LRep): Unit = {
    require(newValue.length == edge.arraySize)
    state(edges(edge)) = newValue
  }


  def buildResult[R](implicit ev: CP <:< ResultBuilder[R]): R =
    cp.asInstanceOf[cp.type with ResultBuilder[R]].buildResult(edgeState)
}

object Calibrator {
  def calibrate[R](cp: CalProblem with ResultBuilder[R],
                    maxIterations: Long = 100000,
                    tol: Double = 1e-12,
                    damping: Double = 0d): (R,ConvergenceStats) = {
    val cal = new Calibrator[cp.type](cp)
    val calState = cal.calibrate(maxIterations,tol,damping)
    (cal.buildResult,calState)
  }
}