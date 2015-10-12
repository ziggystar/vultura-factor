package vultura.factor.inference.conditioned

import vultura.factor.inference.{MargParI, ParFunI, Result}
import vultura.factor.{Factor, LogD, Problem}
import vultura.util._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.ref.SoftReference
import scala.util.Random

/** Very general CBP implementation that supports incremental approximate inference, and other inference algorithms than
  * BP.
  * For a simpler CBP implementation see [[CBP]]
  *
  * @param problem The initial problem.
  * @param hLeaf Leaf selection heuristic.
  * @param hVariable Variable selection heuristic; this value is minimized.
  */
class ConditionedInference[State <: AnyRef,LSI,VSI](val problem: Problem,
                                                    val cbpSolver: CBPSolverPlugin[State] = HybridSolver(ExactSolver.maxMinDegreeJT(2),BPSolverPlugin(maxSteps = 50000)),
                                                    val simplifier: Conditioner = SimpleConditioner,
                                                    val runInitially: Int = 0,
                                                    private val random: Random = new Random(0))(
  val hLeaf: LeafHeuristic[LSI] = HL_MaxZ,
  val hVariable: VariableSelection[VSI] = VariableSelection.HV_MaxDegree)(
  implicit val state2ls: State <:< LSI,
  val state2vs: State <:< VSI) extends MargParI {

  class Leaf(problem: Problem, oldState: Option[State]){
    val (result: Result, info: Option[(SoftReference[State], Double, Set[GCondition])]) = {
      val newResult: Either[MargParI, State] = oldState.map(cbpSolver.incremental(_,problem)).getOrElse(cbpSolver.create(problem))
      newResult match {
        case Left(r)         => (r.toResult, None)
        case Right(newState) => (cbpSolver.result2mpi(newState).toResult, Some((new SoftReference(newState), hLeaf(state2ls(newState),problem), hVariable(state2vs(newState),problem,random))))
      }
    }
    def isExact: Boolean = info.isEmpty
    def h: Double = info.map(_._2).getOrElse(Double.PositiveInfinity)
    def branches: Set[GCondition] = info.get._3
    def branch(c: GCondition): Leaf = new Leaf(simplifier.conditionSimplify(problem,c,Some(result)), info.flatMap(_._1.get))
  }
  
  var iterations: Int = 0

  private val fringe: mutable.PriorityQueue[Leaf] =
    mutable.PriorityQueue(new Leaf(problem, None))(Ordering.by((_: Leaf).isExact).andThen(Ordering.by(_.h)).reverse)

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

  /** @return The number of leafs that are not exactly solved. */
  def openLeafs: Int = fringe.count(!_.isExact)

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def decodedVariableBelief(vi: Int): Factor = {
    val f = fringe.toArray
    val weights = LogD.decode(LogD.normalize(f.map(_.result.logZ)))
    val weightedBeliefs: Array[Array[Double]] = for{
      (l,w) <- f zip weights if w > 0d
    } yield l.result.decodedVariableBelief(vi).values.map(_ * w)
    Factor(Array(vi), weightedBeliefs.transpose.map(_.sum))
  }


  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): Factor = problem.ring.encode(decodedVariableBelief(vi))

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(this.logZ)
  override def logZ: Double = LogD.sumA(fringe.toArray.map(_.result.logZ))
}

trait Conditioner{
  def name: String
  /** @param zeroMarginals Maybe marginals of which only zeros are correct. */
  def conditionSimplify(p: Problem, c: GCondition, zeroMarginals: Option[MargParI] = None): Problem
}

object SimpleConditioner extends Conditioner {
  def simplifyDeterminism(p: Problem, vars: Set[Int]): Problem = {
    val conditions: Set[((Int, Int), Factor)] = for{
      v <- vars
      f <- p.factorsOfVariable(v)
      factorMargs = p.factorsOfVariable(v).map(f => Factor.multiplyRetain(p.ring)(p.domains)(Array(f),Array(v)).normalize(p.ring))
      marg: Factor = Factor.multiplyRetain(p.ring)(p.domains)(factorMargs,Array(v)).normalize(p.ring)
      value <- 0 until p.domains(v) if marg.values(value) == p.ring.one
    } yield (v -> value, marg)
    //todo no check for inconsistency!!!
    //condition all factors and add deterministic singleton factors for the conditioned variables
    val conditionedProblem = p.condition(conditions.map(_._1).toMap).simplify
    conditionedProblem.copy(factors = conditionedProblem.factors ++ conditions.map(_._2))
  }
  def name = "SimpleConditioner"
  override def conditionSimplify(p: Problem, c: GCondition, zeroMarginals: Option[MargParI] = None): Problem = {
    val factors = c.map{case (v,values) =>
      Factor
        .fromFunction(Array(v),p.domains,vs => if(values.contains(vs(0))) p.ring.one else p.ring.zero)
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
  def apply(information: I, p: Problem, rand: Random): Set[GCondition]
}

/** A special type of variable selection that branches on all possible assignments to a set of variables. */
trait CompleteSplitVariableSelection[-I] extends VariableSelection[I]{
  final override def apply(information: I, p: Problem, rand: Random): Set[GCondition] = {
    val v = selectVariable(information,p).pickRandom(rand)
    (0 until p.domains(v)).map(value => Map(v -> Set(value))).toSet
  }
  def selectVariable(information: I, p: Problem): Set[Int]
}

/** Variable selection heuristics that work by assigning a numeric value to each variable and pick an extreme one.
  * @param heuristic Assigns larger values to variables that appear more attractive. */
case class NumericVariableHeuristic[-I](name: String, heuristic: (I,Problem) => Int => Double) extends CompleteSplitVariableSelection[I]{
  override def selectVariable(information: I, p: Problem): Set[Int] = maxByMultiple(p.variables)(heuristic(information,p)).toSet
}

case class MaxMedianHeuristic[-I](heuristics: Seq[NumericVariableHeuristic[I]], quantile: Double) extends CompleteSplitVariableSelection[I]{
  require(quantile >= 0 && quantile < 1)
  /** @return The proportion of the range of values that gets cut off when reducing from 1 to `quantile` quantile.
    * This is larger for "good" heuristics. */
  def quantileRatio(values: Array[Double]): Double = {
    val sorted = values.sorted
    val q: Double = sorted(math.round(math.floor(sorted.size * quantile)).toInt)
    (sorted.last - q)/(sorted.last - sorted.head)
  }
  override def name: String = s"MaxMedian(${heuristics.map(_.name).mkString(",")})"
  override def selectVariable(information: I, p: Problem): Set[Int] = {
    val results: Seq[Array[Double]] = heuristics.map(h => p.variables.map(h.heuristic(information,p))(collection.breakOut): Array[Double])
    val bestH: Array[Double] = results.maxBy(quantileRatio)
    maxByMultiple(p.variables)(bestH).toSet
  }
}

object VariableSelection{
  val HV_MaxDegree = NumericVariableHeuristic("Max Degree", (_: Any,p) => v => p.degreeOfVariable(v).toDouble)
  val HV_TTC = NumericVariableHeuristic[ExtendedBPResult]("TTC", (bp,p) => v =>
    p.factorIdxOfVariable(v).map(f =>
      math.max(bp.lastUpdate(bp.V2FMsg(v,f)), bp.lastUpdate(bp.F2VMsg(f,v)))
    ).max.toDouble)
  val HV_TTC_Mean = NumericVariableHeuristic[ExtendedBPResult]("TTC-Mean", (bp,p) => v =>
    p.factorIdxOfVariable(v).foldLeft(0d){case (acc, f) =>
      acc + bp.lastUpdate(bp.V2FMsg(v,f)) + bp.lastUpdate(bp.F2VMsg(f,v))
    } / p.factorsOfVariable(v).size
  )
  val HV_MinEntropy = NumericVariableHeuristic[MargParI]("MinEntropy", (mp,p) => v =>
    Some(p.ring.entropy(mp.variableBelief(v).values)).filterNot(_ == 0d).getOrElse(Double.NegativeInfinity)
  )
}