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

