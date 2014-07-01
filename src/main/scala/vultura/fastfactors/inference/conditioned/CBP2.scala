package vultura.fastfactors.inference.conditioned

import vultura.fastfactors.inference.{Result, MargParI, ParFunI}
import vultura.fastfactors.{FastFactor, LogD, Problem}
import vultura.util._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.ref.SoftReference

/** Incremental CBP implementation using the cp2.LBP BP implementation.
  *
  * @param problem The initial problem.
  * @param hLeaf Leaf selection heuristic.
  * @param hVariable Variable selection heuristic; this value is minimized.
  */
class ConditionedInference[State <: AnyRef,LSI,VSI](val problem: Problem,
                                                    val cbpSolver: CBPSolverPlugin[State] = HybridSolver(ExactSolver.maxMinDegreeJT(2),BPSolverPlugin(maxSteps = 50000)),
                                                    val simplifier: Conditioner = SimpleConditioner,
                                                    val runInitially: Int = 0)(
  val hLeaf: LeafHeuristic[LSI] = HL_MaxZ,
  val hVariable: VariableSelection[VSI] = HV_MaxDegree)(
  implicit val state2ls: State <:< LSI,
  val state2vs: State <:< VSI) extends MargParI {

  class Leaf(oldProblem: Problem, condition: GCondition, oldState: Option[State]){
    val problem: Problem = simplifier.conditionSimplify(oldProblem,condition)
    val (result: Result, info: Option[(SoftReference[State], Double, Set[GCondition])]) = {
      val newResult: Either[MargParI, State] = oldState.map(cbpSolver.incremental(_,oldProblem,problem)).getOrElse(cbpSolver.create(problem))
      newResult match {
        case Left(r)         => (r.toResult, None)
        case Right(newState) => (cbpSolver.result2mpi(newState).toResult, Some((new SoftReference(newState), hLeaf(state2ls(newState),problem), hVariable(state2vs(newState),problem))))
      }
    }
    def isExact: Boolean = info.isEmpty
    def h: Double = info.map(_._2).getOrElse(Double.PositiveInfinity)
    def branches: Set[GCondition] = info.get._3
    def branch(c: GCondition): Leaf = new Leaf(problem, c, info.map(_._1.get).flatten)
  }
  
  var iterations: Int = 0

  private val fringe: mutable.PriorityQueue[Leaf] =
    mutable.PriorityQueue(new Leaf(problem, Map(), None))(Ordering.by((_: Leaf).isExact).andThen(Ordering.by(_.h)).reverse)

  //run initially
  runFor(runInitially)

  /** Side-effecting function that applies one more step of conditioning.
    * @return `true` if the obtained result is exact, and the computation can be stopped.
    */
  def step(): Boolean = {
    val next = fringe.head
    if(next.isExact)
      true
    else{
      fringe.dequeue()
      iterations = iterations + 1
      val branches: Set[GCondition] = next.branches
      //insert into fringe
      branches.foreach { cond =>
        fringe += next.branch(cond)
      }
      fringe.head.isExact
    }
  }

  @tailrec
  final def run(p: ConditionedInference[_,_,_] => Boolean, acc: Int = 0): Int =
    if(p(this) && !this.isExact) {
      step()
      run(p,acc + 1)
    } else acc

  def runFor(n: Int): Int = {
    val now = iterations
    run(_.iterations < (now + n))
  }

  def isExact: Boolean = fringe.head.isExact
  /** @return returns ratio between 1 and 0. 1 means exact, 0 means all (possible) leafs are estimates. */
  def exactShare: Double = {
    val exactLogZ = LogD.sumA(fringe.toArray.collect{case l if l.isExact => l.result.logZ})
    val approxLogZ = LogD.sumA(fringe.toArray.collect{case l if !l.isExact => l.result.logZ})
    math.exp(exactLogZ - LogD.sum(exactLogZ,approxLogZ))
  }

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def decodedVariableBelief(vi: Int): FastFactor = {
    val f = fringe.toArray
    val weights = LogD.decode(LogD.normalize(f.map(_.result.logZ)))
    val weightedBeliefs: Array[Array[Double]] = for{
      (l,w) <- f zip weights if w > 0d
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
  def simplifyDeterminism(p: Problem, vars: Set[Int]): Problem = {
    val conditions: Set[((Int, Int), FastFactor)] = for{
      v <- vars
      f <- p.factorsOfVariable(v)
      factorMargs = p.factorsOfVariable(v).map(f => FastFactor.multiplyRetain(p.ring)(p.domains)(Array(f),Array(v)).normalize(p.ring))
      marg: FastFactor = FastFactor.multiplyRetain(p.ring)(p.domains)(factorMargs,Array(v)).normalize(p.ring)
      value <- 0 until p.domains(v) if marg.values(value) == p.ring.one
    } yield (v -> value, marg)
    //todo no check for inconsistency!!!
    val conditionedProblem = p.condition(conditions.map(_._1).toMap).simplify
    conditionedProblem.copy(factors = conditionedProblem.factors ++ conditions.map(_._2))
  }
  def name = "SimpleConditioner"
  override def conditionSimplify(p: Problem, c: GCondition): Problem = {
    val factors = c.map{case (v,values) =>
      FastFactor
        .fromFunction(Array(v),p.domains,vs => if(values.contains(vs(0))) p.ring.one else p.ring.zero)
        .normalize(p.ring)
    }
    simplifyDeterminism(p.copy(factors = p.factors ++ factors), c.keySet)
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