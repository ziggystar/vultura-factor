package vultura.calibration

import com.typesafe.scalalogging.StrictLogging
import vultura.factor.inference.ConvergenceStats
import vultura.util.{FastBitSet, OpenBitSet, SIIndex}
import FastBitSet._
import vultura.factor.inference.conditioned.CBP.CLAMP_METHOD

import collection.mutable
import scala.collection.immutable.IndexedSeq

/** Mutable class that holds a calibration state. */
class Calibrator[CP <: CalProblem](val cp: CP) extends StrictLogging {
  /** Internal representation of edge state. */
  type IR = Array[Double]
  type N = cp.N
  type CN = cp.ComputedNode
  type PN = cp.ParameterNode

  /** An node index. */
  type NI = Int

  protected val nodes: SIIndex[N] = new SIIndex(cp.nodes)

  protected var edgeLastUpdate: Array[Long] = null
  protected var edgeTotalUpdates: Array[Long] = null
  protected var edgeLastDiff: Array[Double] = null
  protected var edgeTotalDiff: Array[Double] = null
  protected var doEdgeUpdates: Boolean = false

  protected var state: Array[IR] = _
  protected val nodeConverged: OpenBitSet = {
    val bs = new OpenBitSet(nodes.size)
    bs.clear(0,nodes.size)
    bs
  }
  protected val (dependencies,successors): (Array[Array[NI]], Array[Array[NI]]) = {
    val deps: Array[Array[NI]] =
      nodes.elements.map(_.dependenciesOpt.getOrElse(IndexedSeq[N]()).map(nodes.forward)(collection.breakOut): Array[NI])(collection.breakOut)
    val succs: IndexedSeq[mutable.Set[NI]] = nodes.indices.map(_ => mutable.Set[NI]())
    for{
      n <- deps.indices
      pred <- deps(n)
    } {
      succs(pred) += n
    }
    (deps,succs.map(_.toArray.sorted)(collection.breakOut))
  }

  /** Sets the parameters of the problem, and invalidates edge convergence states. */
  def initialize(parameters: CP#Parameter): Unit = {
    val initializer = cp.initializer(parameters.asInstanceOf[cp.Parameter])

    state = nodes.elements.map(n => {
      val newVal = initializer(n)
      require(newVal.length == n.arraySize, "initializing with array of wrong size")
      newVal
    })(collection.breakOut)

    nodeConverged.clear(0,nodes.size)

    //set all parameter nodes to be converged
    nodes.indices.foreach{ ni =>
      if(nodes.backward(ni).isInstanceOf[PN]){
        nodeConverged.fastSet(ni)
      }
    }
  }

  private final def newNodeValue(ei: NI): Array[Double] = {
    //update edge
    val node = nodes.backward(ei)
    node match {
      case n: PN =>
        throw new RuntimeException("Parameter nodes must already be initialized; this must be a bug in Calibrator")
      case n: CN =>
        val newValue = new Array[Double](node.arraySize)
        n.compute(dependencies(ei).map(state)(collection.breakOut), newValue)
        newValue
    }
  }

  protected def calibrateComponent( component: Array[Int]
                                  , maxIterations: Long
                                  , maxDiff: Double
                                  , damping: Double = 0d): ConvergenceStats = {
    val componentNodes: IndexedSeq[NI] = component.toIndexedSeq.sorted
    var iteration = -1L //we need one iteration for asserting convergence
    var iterationDiff: Double = 0d
    do {
      iterationDiff = 0d

      //component node index
      var cni = 0
      while(cni < componentNodes.size){
        val ei: NI = componentNodes(cni)
        if(!nodeConverged.fastGet(ei)) {
          //update edge
          val oldValue = state(ei)
          val newValue = newNodeValue(ei)

          var newDiff = 0d
          var point = 0  //loop index going over the value array
          val dampingCompl = 1d - damping
          while (point < newValue.length) {
            if(damping > 0)
              newValue(point) = dampingCompl * newValue(point) + damping * oldValue(point)
            newDiff = math.max(newDiff, math.abs(if(newValue(point).isNegInfinity && oldValue(point).isNegInfinity) 0d else newValue(point) - oldValue(point)))
            point += 1
          }
          //only update the node if necessary
          if(newDiff >= maxDiff) {
            state(ei) = newValue
            successors(ei).foreach(nodeConverged.fastClear)
            //only store per edge stats if requested
            if(doEdgeUpdates){
              edgeTotalUpdates(ei) += 1
              edgeTotalDiff(ei)    += newDiff
              edgeLastDiff(ei)     =  newDiff
              edgeLastUpdate(ei)   = iteration
            }
          }
          if(damping == 0 || newDiff < maxDiff)
            nodeConverged.fastSet(ei)
          iterationDiff = math.max(iterationDiff, newDiff.ensuring(!_.isNaN))
        }
        cni += 1
      }

      iteration += 1
    } while (!(iterationDiff < maxDiff) && iteration < maxIterations)

    val convStats = ConvergenceStats(iteration,iterationDiff,iterationDiff < maxDiff)
    logger.debug(s"calibrated cyclic component of size ${component.length} with maxIt: $maxIterations, maxDiff: $maxDiff, damping: $damping: " + convStats)
    convStats.ensuring(!_.maxDiff.isNaN)
  }

  /** Calibrate (part of) the problem.
    *
    * @param maxIterations Maximum number of updates per edge.
    * @param maxDiff Consider differences below this threshold as converged.
    * @param damping Zero means no damping.
    * @param sccDecomposition Decompose the edges into strongly connected components.
    *                         Setting this option to true insures linear runtime for tree-structured problems.
    *                         If true, `maxIterations` applies to each component.
    * @param edges Limit updates to these edges.
    * @return
    */
  def calibrate(maxIterations: Long,
                maxDiff: Double,
                damping: Double = 0d,
                sccDecomposition: Boolean = true,
                edges: Iterable[N] = nodes.elements): ConvergenceStats = {

    require(state != null, "calibrator has to be initialized prior to calibration")
    require(damping >= 0 && damping < 1d, s"damping has to be within [0,1[, but is $damping")

    if(sccDecomposition) {
      val stronglyConnectedComponents: IndexedSeq[Set[NI]] = {
        import vultura.util.graph._
        val allNodes: Set[NI] = edges.map(nodes.forward)(collection.breakOut)
        val graph = LabeledGraph.fromChildList(
          allNodes,
          allNodes.map(ei => ei -> dependencies(ei).filter(allNodes).toSet)(collection.breakOut): Map[NI, Set[NI]]
        )
        graph.tarjanSCC.toIndexedSeq
      }

      stronglyConnectedComponents
        .map(_.toArray.sorted)
        .map(edges =>
          if(edges.length == 1) {
            //compute the result for single-node components right away
            //but only calculate if the value is not already valid (as for parameters)
            if(!nodeConverged.fastGet(edges.head)) {
              state(edges.head) = newNodeValue(edges.head)
              if(doEdgeUpdates){
                edgeTotalUpdates(edges.head) += 1
                edgeLastDiff(edges.head)      = 0d
                edgeLastUpdate(edges.head)    = 0
              }
            }
            ConvergenceStats(1, 0d, isConverged = true)
          }
          else calibrateComponent(edges, maxIterations, maxDiff, damping)
        ).reduce(_ max _)
    } else {
      calibrateComponent(edges.map(nodes.forward).toArray.distinct.sorted, maxIterations, maxDiff, damping)
    }
  }

  def calibrateExtended(maxIterations: Long,
                        maxDiff: Double,
                        damping: Double = 0d,
                        sccDecomposition: Boolean = true,
                        edges: Iterable[N] = nodes.elements): (ConvergenceStats, Map[CP#ComputedNode,EdgeInfo]) = {
    //initialize storage for per-edge stats
    this.edgeLastUpdate = new Array(nodes.size)
    this.edgeTotalUpdates = new Array(nodes.size)
    this.edgeLastDiff = new Array(nodes.size)
    this.edgeTotalDiff = new Array(nodes.size)
    this.doEdgeUpdates = true
    //run calibration; it checks whether the arrays for the per-edge stats are non-null
    val stats = calibrate(maxIterations, maxDiff, damping, sccDecomposition, edges)
    doEdgeUpdates = false
    val edgeStats: Map[CP#ComputedNode, EdgeInfo] = cp.nodes.asInstanceOf[Set[CP#Node]].collect{
          case n: CP#ComputedNode =>
            val ni = nodes.forward(n.asInstanceOf[this.N])
            n -> EdgeInfo(totalUpdates = edgeTotalUpdates(ni), lastUpdate = edgeLastUpdate(ni), lastDiff = edgeLastDiff(ni), totalDiff = edgeTotalDiff(ni) )
        }(collection.breakOut)
    stats -> edgeStats
  }

  def nodeState(node: N): IR = state(nodes(node))
  def updateState(node: N, newValue: IR): Unit = {
    require(newValue.length == node.arraySize)
    state(nodes(node)) = newValue
  }

  def buildResult[R](implicit ev: CP <:< ResultBuilder[R]): R =
    cp.asInstanceOf[cp.type with ResultBuilder[R]].buildResult(nodeState)
}

/** Holds information wrt a single edge within a computation graph.
  * @param totalUpdates Number of total updates this edge has received.
  * @param lastUpdate Iteration during which the last update was made.
  * @param lastDiff Difference between last and second to last update.
  * @param totalDiff Sum of differences over all updates of this edge.
  */
case class EdgeInfo(totalUpdates: Long, lastUpdate: Long, lastDiff: Double, totalDiff: Double)

/**
  * Created by thomas on 20.04.16.
  */
object Calibrator {
  def calibrateParam[R,P](cp: CalProblem.Aux[P] with ResultBuilder[R],
                     parameters: P,
                     maxIterations: Long = 100000,
                     tol: Double = 1e-12,
                     damping: Double = 0d): (R,ConvergenceStats) = {
    val cal = new Calibrator(cp)
    cal.initialize(parameters)
    val calState = cal.calibrate(maxIterations,tol,damping)
    (cal.buildResult,calState)
  }

  def calibrate[R](cp: CalProblem.Aux[Unit] with ResultBuilder[R],
                   maxIterations: Long = 100000,
                   tol: Double = 1e-12,
                   damping: Double = 0d): (R,ConvergenceStats) = calibrateParam[R,Unit](cp,Unit,maxIterations,tol,damping)
}
