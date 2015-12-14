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

  /** The type of the parameter value that has to be provided to initialize the problem;
    * e.g. assign values to the source nodes. */
  type Parameter

  /** Node type. A node is a representation of a node within the computation graph, but also carries the
    * computation rule and the set of dependencies. */
  trait Node {
    /** Size of the array required to store the state of this edge. */
    def arraySize: Int
    def dependencies: IndexedSeq[N]
    /**
     * - first parameter: `zip`s with `dependencies`.
     * - second parameter: Result of computation shall be stored here. Content of result is garbage.
      */
    def compute(ins: Array[IR], result: IR): Unit
  }

  /** Constructs a new initial value for each edge. */
  def initializer(param: Parameter): N => IR

  /** The set of nodes defined by this problem. */
  def nodes: Set[N]

  def computationGraph: DotGraph[N,(N,N)] =
    DotGraph[N,(N,N)](nodes, for (e <- nodes; d <- e.dependencies) yield (d, e)).labelNodes{case e => e.toString}
}

object CalProblem {
  type Aux[P] = CalProblem {type Parameter = P}
}

trait ResultBuilder[R] {outer: CalProblem =>
  def buildResult(valuation: outer.N => IR): R
}

/** Mutable class that holds a calibration state. */
class Calibrator[P,CP <: CalProblem.Aux[P]](val cp: CP) extends StrictLogging {
  /** Internal representation of edge state. */
  type IR = Array[Double]
  type N = cp.N

  /** An node index. */
  type NI = Int

  protected val nodes: SIIndex[N] = new SIIndex(cp.nodes)
  protected var state: Array[IR] = _
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
    //TODO this is a bit hacky, we do not update edges that have no dependencies,
    // since those should be initialized to their correct value;
    // it would be better if the CalProb interface makes this more clear
    def newNodeValue(ei: NI): Array[Double] = {
      //update edge
      val node = nodes.backward(ei)
      if(node.dependencies.isEmpty)
        state(ei)
      else {
        val newValue = new Array[Double](node.arraySize)
        node.compute(dependencies(ei).map(state)(collection.breakOut), newValue)
        newValue
      }
    }

    val component = stronglyConnectedComponents(componentIndex)
    if(component.size == 1) {
      //just update the edge
      val ni = component.head
      val newVal: IR = newNodeValue(ni)
      require(newVal.size == state(ni).size)
      state(ni) = newVal
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
    require(state != null, "calibrator has to be initialized prior to calibration")
    require(damping >= 0 && damping < 1d, s"damping has to be within [0,1[, but is $damping")
    stronglyConnectedComponents.indices.map(scc => calibrateComponent(scc,maxIterations,maxDiff,damping)).reduce(_ max _)
  }

  def initialize(parameters: P): Unit = {
    state = nodes.elements.map(n => {
      val newVal = cp.initializer(parameters)(n)
      require(newVal.length == n.arraySize, "initializing with array of wrong size")
      newVal
    })(collection.breakOut)
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
  def calibrateParam[R,P](cp: CalProblem.Aux[P] with ResultBuilder[R],
                     parameters: P,
                     maxIterations: Long = 100000,
                     tol: Double = 1e-12,
                     damping: Double = 0d): (R,ConvergenceStats) = {
    val cal = new Calibrator[P,cp.type](cp)
    cal.initialize(parameters)
    val calState = cal.calibrate(maxIterations,tol,damping)
    (cal.buildResult,calState)
  }

  def calibrate[R](cp: CalProblem.Aux[Unit] with ResultBuilder[R],
                   maxIterations: Long = 100000,
                   tol: Double = 1e-12,
                   damping: Double = 0d): (R,ConvergenceStats) = calibrateParam[R,Unit](cp,Unit,maxIterations,tol,damping)
}