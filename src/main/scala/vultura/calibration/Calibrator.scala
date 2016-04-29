package vultura.calibration

import com.typesafe.scalalogging.StrictLogging
import vultura.factor.inference.ConvergenceStats
import vultura.util.{FastBitSet, OpenBitSet, SIIndex}
import FastBitSet._

import collection.mutable
import scala.collection.immutable.IndexedSeq

/** Mutable class that holds a calibration state. */
class Calibrator[P,CP <: CalProblem.Aux[P]](val cp: CP) extends StrictLogging {
  /** Internal representation of edge state. */
  type IR = Array[Double]
  type N = cp.N
  type CN = cp.ComputedNode
  type PN = cp.ParameterNode

  /** An node index. */
  type NI = Int

  protected val nodes: SIIndex[N] = new SIIndex(cp.nodes)
  protected var state: Array[IR] = _
  protected val nodeConverged: OpenBitSet = {
    val bs = new OpenBitSet(nodes.size)
    bs.clear(0,nodes.size)
    bs
  }
  protected val (dependencies,successors): (Array[Array[NI]], Array[Array[NI]]) = {
    val deps: Array[Array[NI]] = nodes.elements.map(_.dependenciesOpt.getOrElse(IndexedSeq[N]()).map(nodes.forward(_))(collection.breakOut): Array[NI])(collection.breakOut)
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
  def initialize(parameters: P): Unit = {
    val initializer = cp.initializer(parameters)

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

  protected def calibrateComponent(component: Array[Int], maxIterations: Long, maxDiff: Double, damping: Double = 0d): ConvergenceStats = {
    def newNodeValue(ei: NI): Array[Double] = {
      //update edge
      val node = nodes.backward(ei)
      node match {
        case n: PN =>
          throw new RuntimeException("this point should not be reached")
        case n: CN =>
          val newValue = new Array[Double](node.arraySize)
          n.compute(dependencies(ei).map(state)(collection.breakOut), newValue)
          newValue
      }
    }

    val componentNodes: IndexedSeq[NI] = component.toIndexedSeq.sorted
    var iteration = -1L //we need one iteration for asserting convergence
    var iterationDiff: Double = 0d
    do {
      iterationDiff = 0d

      //component node index
      var cni = 0
      while(cni < componentNodes.size){
        val ei = componentNodes(cni)
        if(!nodeConverged.fastGet(ei)) {
          //update edge
          val oldValue = state(ei)
          val newValue = newNodeValue(ei)

          var newDiff = 0d
          var point = 0  //loop index going over the value array
          while (point < newValue.length) {
            newDiff = math.max(newDiff, math.abs(newValue(point) - oldValue(point)))
            newValue(point) = (1 - damping) * newValue(point) + damping * oldValue(point)
            point += 1
          }
          //only update the node if necessary
          if(newDiff >= maxDiff) {
            state(ei) = newValue
            successors(ei).foreach(nodeConverged.fastClear)
          }
          nodeConverged.fastSet(ei)
          iterationDiff = math.max(iterationDiff, newDiff)
        }
        cni += 1
      }

      iteration += 1
    } while (iterationDiff > maxDiff && iteration < maxIterations)

    ConvergenceStats(iteration,iterationDiff,iterationDiff < maxDiff)
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
        .map(calibrateComponent(_, maxIterations, maxDiff, damping))
        .reduce(_ max _)
    } else {
      calibrateComponent(edges.map(nodes.forward).toArray.distinct.sorted, maxIterations, maxDiff, damping)
    }
  }

  def nodeState(node: N): IR = state(nodes(node))
  def updateState(node: N, newValue: IR): Unit = {
    require(newValue.length == node.arraySize)
    state(nodes(node)) = newValue
  }

  def buildResult[R](implicit ev: CP <:< ResultBuilder[R]): R =
    cp.asInstanceOf[cp.type with ResultBuilder[R]].buildResult(nodeState)
}


/**
  * Created by thomas on 20.04.16.
  */
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
