package vultura.fastfactors.inference.conditioned

import vultura.fastfactors.{FastFactor, Problem}
import vultura.fastfactors.inference.cp2.{BPResult, MaxDiff, MutableFIFOCalibrator, LBP}
import vultura.fastfactors.inference.{VariableOrderer, JunctionTree, MargParI}
import vultura.util.TreeWidth

/** A `CBPSolverPlugin` handles the inference within an instance of [[ConditionedInference]].
  * This trait offers the possibility to perform incremental inference using the `incremental` method.
  */
trait CBPSolverPlugin[S]{
  def name: String
  implicit def result2mpi: S <:< MargParI
  def create(p: Problem): Either[MargParI,S]
  def incremental(oldState: S, oldProblem: Problem, newProblem: Problem): Either[MargParI,S] = create(newProblem)
}

/** A CBPSolverPlugin that is build through a combination of an exact and an (incremental) approximate solver. */
case class HybridSolver[S](exact: ExactSolver, approximate: ApproximateSolver[S]) extends CBPSolverPlugin[S]{
  override def name: String = s"Hyb(${exact.name},${approximate.name})"
  override implicit def result2mpi: <:<[S, MargParI] = approximate.result2mpi
  override def create(p: Problem): Either[MargParI, S] =
    exact.attempt(p).map(Left(_)).getOrElse(Right(approximate.create(p)))
  override def incremental(oldState: S, oldProblem: Problem, newProblem: Problem): Either[MargParI, S] =
    exact.attempt(newProblem).map(Left(_)).getOrElse(Right(approximate.increment(oldState, oldProblem, newProblem)))
}

/** Attempts to solve a problem exactly. Currently no means of threading state. */
trait ExactSolver{
  def name: String
  def attempt(p: Problem): Option[MargParI]
}

object ExactSolver{
  /** Solves the problem if the tree width is at most `maxWidth`. */
  def maxMinDegreeJT(maxWidth: Int): ExactSolver = new ExactSolver {
    override val name: String = s"Max-MinDegree-JT($maxWidth)"
    override def attempt(p: Problem): Option[MargParI] = {
      val (ordering,width) = TreeWidth.minDegreeOrderingAndWidth(p.factors.map(_.variables.toSet))
      if(width <= maxWidth) Some(new JunctionTree(p, VariableOrderer.fromOrder(ordering)))
      else None
    }
  }
}

trait ApproximateSolver[S]{
  def name: String
  def result2mpi: S <:< MargParI
  def create(p: Problem): S
  def increment(oldState: S, oldProblem: Problem, newProblem: Problem): S
}
