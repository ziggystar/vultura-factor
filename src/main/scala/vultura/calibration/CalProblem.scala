package vultura.calibration

import com.typesafe.scalalogging.StrictLogging
import vultura.factor.inference.ConvergenceStats
import vultura.util.SIIndex
import vultura.util.graph.graphviz.DotGraph

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
  /** Internal representation of values. Only arrays of doubles. */
  type IR = Array[Double]

  /** Node type exposed by a problem. This defined the signature methods `initializer` and `nodes`. */
  type N <: Node

  /** Node type. A node is a representation of a node within the computation graph, but also carries the
    * computation rule and the set of dependencies. */
  trait Node {
    type D <: N
    /** Size of the array required to store the state of this edge. */
    def arraySize: Int
    def dependencies: IndexedSeq[D]
    /**
     * - first parameter: `zip`s with `dependencies`.
     * - second parameter: Result of computation shall be stored here. Content of result is garbage.
      */
    def compute(ins: Array[IR], result: IR): Unit
  }

  /** Constructs a new initial value for each edge. */
  def initializer: N => IR

  /** The set of nodes defined by this problem. */
  def nodes: Set[N]

  def computationGraph: DotGraph[N,(N,N)] =
    DotGraph[N,(N,N)](nodes, for (e <- nodes; d <- e.dependencies) yield (d, e)).labelNodes{case e => e.toString}
}

trait ResultBuilder[R] {outer: CalProblem =>
  def buildResult(valuation: outer.N => IR): R
}

/** Mutable class that holds a calibration state. */
class Calibrator[CP <: CalProblem](val cp: CP) extends StrictLogging {
  /** Internal representation of edge state. */
  type IR = Array[Double]
  type N = cp.N

  /** An node index. */
  type NI = Int

  protected val nodes: SIIndex[N] = new SIIndex(cp.nodes)
  protected var state: Array[IR] = nodes.elements.map(cp.initializer)(collection.breakOut)
  protected val dependencies: IndexedSeq[IndexedSeq[NI]] = nodes.elements.map(_.dependencies.map(nodes(_)))

  val stronglyConnectedComponents: IndexedSeq[Set[NI]] = {
    import vultura.util.graph._
    val graph = LabeledGraph.fromChildList(
      nodes.indices.toSet,
      nodes.indices.map(ei => ei -> dependencies(ei).toSet)(collection.breakOut): Map[NI, Set[NI]]
    )
    graph.tarjanSCC.toIndexedSeq
  }

  protected def calibrateComponent(componentIndex: Int, maxIterations: Long, maxDiff: Double, damping: Double = 0d): ConvergenceStats = {
    def newNodeValue(ei: NI): Array[Double] = {
      //update edge
      val node = nodes.backward(ei)
      val newValue = new Array[Double](node.arraySize)
      node.compute(dependencies(ei).map(state)(collection.breakOut), newValue)
      newValue
    }

    val component = stronglyConnectedComponents(componentIndex)
    if(component.size == 1) {
      //just update the edge
      val ni = component.head
      state(ni) = newNodeValue(ni)
      ConvergenceStats(iterations = 1, maxDiff = 0, isConverged = true)
    } else {
      val componentNodes: IndexedSeq[NI] = component.toIndexedSeq
      var iteration = 0L
      var iterationDiff: Double = 0d
      do {
        iterationDiff = 0d

        var cni = 0
        while(cni < componentNodes.size){
          val ei = componentNodes(cni)

          //update edge
          val oldValue = state(ei)
          val newValue = newNodeValue(ei)

          var newDiff = 0d
          var point = 0
          while(point < newValue.length){
            newDiff = math.max(newDiff,math.abs(newValue(point) - oldValue(point)))
            newValue(point) = (1-damping)*newValue(point) + damping * oldValue(point)
            point += 1
          }
          iterationDiff = math.max(iterationDiff,newDiff)
          state(ei) = newValue
          cni += 1
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
    state = nodes.elements.map(cp.initializer)(collection.breakOut)
  }

  def nodeState(node: N): IR = state(nodes(node))
  def updateState(node: N, newValue: IR): Unit = {
    require(newValue.length == node.arraySize)
    state(nodes(node)) = newValue
  }

  def buildResult[R](implicit ev: CP <:< ResultBuilder[R]): R =
    cp.asInstanceOf[cp.type with ResultBuilder[R]].buildResult(nodeState)
}

object Calibrator {
  def calibrate[R](cp: CalProblem with ResultBuilder[R],
                    maxIterations: Long = 100000,
                    tol: Double = 1e-12,
                    damping: Double = 0d): (R,ConvergenceStats) = {
    val cal = new Calibrator(cp)
    val calState = cal.calibrate(maxIterations,tol,damping)
    (cal.buildResult,calState)
  }
}