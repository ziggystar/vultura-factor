package vultura.calibration

import com.typesafe.scalalogging.StrictLogging
import vultura.factor.inference.ConvergenceStats
import vultura.util.SIIndex

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
