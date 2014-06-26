package vultura.fastfactors.inference.conditioned

import vultura.fastfactors.inference.{ParFunI, VariableOrderer, JunctionTree, MargParI}
import vultura.fastfactors.inference.cp2.{BPResult, MaxDiff, LBP, MutableFIFOCalibrator}
import vultura.fastfactors.{LogD, FastFactor, Problem}
import vultura.util.TreeWidth

import scala.collection.mutable

/** Incremental CBP implementation using the cp2.LBP BP implementation.
  *
  * @param problem The initial problem.
  * @param hLeaf Leaf selection heuristic.
  * @param hVariable Variable selection heuristic; this value is minimized.
  */
class ConditionedInference[State,LSI,VSI](val problem: Problem,
                                          val cbpSolver: CBPSolverPlugin[State] = HybridSolver(ExactSolver.maxMinDegreeJT(2),BPSolverPlugin()),
                                          val simplifier: Conditioner = SimpleConditioner,
                                          val runInitially: Int = 0)(
  val hLeaf: LeafHeuristic[LSI] = HL_MaxZ,
  val hVariable: VariableSelection[VSI] = HV_MaxDegree)(
  implicit val state2ls: State <:< LSI,
           val state2vs: State <:< VSI) extends MargParI {

  case class Leaf(p: Problem, state: Either[MargParI,State]){
    val h = state.fold(_ => Double.PositiveInfinity, hLeaf(_, p))
    def result: MargParI = state.fold(identity,cbpSolver.result2mpi)
    def isExact: Boolean = state.isLeft
  }
  
  var iterations: Int = 0

  private val fringe: mutable.PriorityQueue[Leaf] =
    mutable.PriorityQueue(Leaf(problem,cbpSolver.create(problem)))(Ordering.by(_.h))

  //run initially
  runFor(runInitially)

  /** Side-effecting function that applies one more step of conditioning.
    * @return `true` if the obtained result is exact, and the computation can be stopped.
    */
  def step(): Boolean = {
    val next = fringe.head
    next.state match {
      case Left(_) => true //only exact leaves remaining
      case Right(state) =>
        fringe.dequeue()
        iterations = iterations + 1
        val branches: Set[GCondition] = hVariable(state,next.p)
        //apply condition and simplify
        val simplified: Seq[Problem] = branches.toSeq.map(cond => simplifier.conditionSimplify(next.p,cond))
        //insert into fringe
        simplified.foreach { newProblem =>
          fringe += Leaf(newProblem, cbpSolver.incremental(state, next.p, newProblem))
        }
        fringe.head.isExact
    }
  }

  def run(p: ConditionedInference[_,_,_] => Boolean, acc: Int = 0): Int =
    if(p(this) && !isExact) {
      step()
      run(p,acc + 1)
    } else acc

  def runFor(n: Int): Int = {
    val now = iterations
    run(_.iterations < (now + n))
  }

  def isExact: Boolean = fringe.size == fringe.count(_.isExact)
  /** @return returns ratio between 1 and 0. 1 means exact, 0 means all (possible) leafs are estimates. */
  def exactShare: Double = {
    val exactLogZ = LogD.sumA(fringe.toArray.collect{case l if l.isExact => l.result.logZ})
    val approxLogZ = LogD.sumA(fringe.toArray.collect{case l if l.isExact => l.result.logZ})
    math.exp(exactLogZ - LogD.sum(exactLogZ,approxLogZ))
  }

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def decodedVariableBelief(vi: Int): FastFactor = {
    val f = fringe.toArray
    val weights = LogD.decode(LogD.normalize(f.map(_.result.logZ)))
    val weightedBeliefs: Array[Array[Double]] = for{
      (l,w) <- f zip weights
    } yield l.result.decodedVariableBelief(vi).values.map(_ * w)
    FastFactor(Array(vi), weightedBeliefs.transpose.map(_.sum))
  }


  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): FastFactor = problem.ring.encode(decodedVariableBelief(vi))

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(this.logZ)
  override def logZ: Double = LogD.sumA(fringe.toArray.map(_.result.logZ))
}

trait Conditioner{
  def name: String
  def conditionSimplify(p: Problem, c: GCondition): Problem
}

object SimpleConditioner extends Conditioner {
  def name = "SimpleConditioner"
  override def conditionSimplify(p: Problem, c: GCondition): Problem = {
    val factors = c.map{case (v,values) =>
      FastFactor
        .fromFunction(Array(v),p.domains,vs => if(values.contains(vs(0))) p.ring.one else p.ring.zero)
        .normalize(p.ring)
    }
    p.copy(factors = p.factors ++ factors).simplify
  }
}





trait LeafHeuristic[-I] {
  def name: String
  def apply(information: I, p: Problem): Double
}

object HL_MaxZ extends LeafHeuristic[ParFunI]{
  override def name: String = "Max Z"
  override def apply(information: ParFunI, p: Problem): Double = -information.logZ
}
object HL_MaxVar extends LeafHeuristic[Any]{
  override def name: String = "Max Var"
  override def apply(information: Any, p: Problem): Double = -p.variables.size.toDouble
}

trait VariableSelection[-I] {
  def name: String
  def apply(information: I, p: Problem): Set[GCondition]
}

object HV_MaxDegree extends VariableSelection[Any]{
  override def name: String = "Max Degree"
  override def apply(information: Any, p: Problem): Set[GCondition] = {
    val v = p.variables.maxBy(v => p.degreeOfVariable(v))
    (0 until p.domains(v)).map(value => Map(v -> Set(value))).toSet
  }
}


object ConditionedInference{

  //object HV_MaxDegree extends VariableSelection[]
}