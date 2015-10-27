package vultura.factor.inference.cal2

import vultura.factor.inference.MargParI
import vultura.factor.{Problem, SumProductTask}
import vultura.util.{SIIndex, SSet}

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

  /** Edge type. */
  trait Edge {
    type D <: Edge
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
  def initializer: Edge => LRep

  def edges: Set[Edge]
}

/** Mutable class that holds a calibration state. */
class Calibrator(val cp: CalProblem) {
  type LRep = CalProblem#LRep

  /** An edge index. */
  type EI = Int

  protected val edges: SIIndex[cp.Edge] = new SIIndex(cp.edges)
  protected var state: Array[LRep] = edges.elements.map(cp.initializer)(collection.breakOut)
  protected val dependencies: IndexedSeq[IndexedSeq[EI]] = edges.elements.map(_.dependencies.map(edges(_)))

  def calibrate(maxIterations: Long, maxDiff: Double, damping: Double = 0d): (Long, Double, Boolean) = {
    require(damping >= 0 && damping < 1d, s"damping has to be within [0,1[, but is $damping")
    var iteration = 0L
    var diff: Double = 0d
    do {
      diff = 0d

      var ei = 0
      while(ei < edges.size){
        //update edge
        val edge = edges.backward(ei)
        val result = new Array[Double](edge.arraySize)
        edge.compute(dependencies(ei).map(state)(collection.breakOut), result)
        val oldValue = state(ei)

        var newDiff = 0d
        var point = 0
        while(point < result.length){
          newDiff = math.max(newDiff,math.abs(result(point) - oldValue(point)))
          result(point) = (1-damping)*result(point) + damping * oldValue(point)
          point += 1
        }
        diff = math.max(diff,newDiff)
        ei += 1
      }

      iteration += 1
    } while (diff > maxDiff && iteration < maxIterations)

    (iteration,diff,diff < maxDiff)
  }

  def reset(): Unit = {
    state = edges.elements.map(cp.initializer)(collection.breakOut)
  }

  def edgeState(edge: cp.Edge): LRep = state(edges(edge))
  def updateState(edge: cp.Edge, newValue: LRep): Unit = {
    require(newValue.length == edge.arraySize)
    state(edges(edge)) = newValue
  }
}

/** Belief propagation on the factor graph representation. */
case class BetheProblem(ps: Problem) extends CalProblem {
  require(
    new SSet[Int](ps.scopeOfFactor.map(_.toSet).toSet).maximalSets.size == ps.factors.size,
    "no factor scope may be subset of another one, simplify problem first")
  type VI = ps.VI

  type FI = ps.FI

  val aggregateFactors = new SSet[Int](ps.scopeOfFactor.map(_.toSet).toSet)

  trait FactorEdge extends Edge {
    def variable: Int
    val arraySize = ps.domains(variable)
  }

  case class V2F(variable: VI, factor: FI) extends FactorEdge{
    type D = F2V
    lazy val dependencies: IndexedSeq[F2V] = ps.factorIdxOfVariable(variable).filterNot(_ == factor).map(F2V(_,variable))
    lazy val task: (Array[LRep], LRep) => Unit = SumProductTask(Array(variable),ps.domains,Array(), ps.ring).sumProductNormalize(_,_)
    override def compute(ins: Array[LRep], result: LRep): Unit = task(ins,result)
  }

  case class F2V(factor: FI, variable: VI) extends FactorEdge{
    override type D = V2F
    lazy val dependencies: IndexedSeq[V2F] = ps.scopeOfFactor(factor).filterNot(_ == variable).map(V2F(_,factor))
    lazy val task = SumProductTask(
      Array(variable),
      ps.domains,
      dependencies.map(d => Array(d.variable)).toArray :+ ps.scopeOfFactor(factor),
      ps.ring
    )
    private lazy val resultHolder: Array[LRep] = {
      val r = new Array[LRep](dependencies.size + 1)
      r(dependencies.size - 1) = ps.factors(factor).values
      r
    }
    override def compute(ins: Array[LRep], result: LRep): Unit = {
      System.arraycopy(ins,0,resultHolder,0,ins.length)
      task.sumProductNormalize(resultHolder,result)
    }
  }

  override def edges: Set[Edge] =
    (for (vi <- ps.variables; fi <- ps.factorIdxOfVariable(vi); e <- Seq(F2V(fi, vi), V2F(vi, fi))) yield e)(collection.breakOut)

  /** Constructs a new initial value for each edge. */
  override def initializer: (Edge) => LRep = e => Array.fill[Double](e.arraySize)(ps.ring.one)

  def buildResult(valuation: Edge => LRep): MargParI = ???
}
