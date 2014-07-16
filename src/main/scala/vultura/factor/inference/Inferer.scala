package vultura.factor.inference

import vultura.factor._

/** Base-trait for probabilistic inference algorithms. */
trait Inferer {
  def problem: Problem
}

/** Trait that is implemented by inference algorithms that can compute variable marginals. */
trait MarginalI extends Inferer {
  def decodedVariableBelief(vi: Int): Factor =
    if(problem.ring != NormalD) problem.ring.decode(variableBelief(vi)) else variableBelief(vi)
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): Factor
  /** @return marginal distribution of variable in log encoding. */
  def logVariableBelief(vi: Int): Factor  =
    if(problem.ring == LogD) variableBelief(vi) else LogD.encode(decodedVariableBelief(vi))
}

/** Trait that is implemented by inference algorithms that can compute the partition function. */
trait ParFunI extends Inferer {
  /** @return Natural logarithm of partition function. */
  def logZ: Double = problem.ring.log(Z)
  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double
  def decodedZ: Double = problem.ring.decode(Array(Z))(0)
}

trait MargParI extends MarginalI with ParFunI{
  def toResult = new Result(this)
}

trait JointMargI extends MarginalI {
  /** Throws if no clique contains `vars`.
    * @return Normalized belief over given variables in encoding specified by problem ring. */
  def cliqueBelief(vars: Array[Var]): Factor

  def decodedCliqueBelief(vars: Array[Var]): Factor =
    if(problem.ring != NormalD) problem.ring.decode(cliqueBelief(vars)) else cliqueBelief(vars)

  /** @return marginal distribution of variable in log encoding. */
  def logCliqueBelief(vars: Array[Var]): Factor  =
    if(problem.ring == LogD) cliqueBelief(vars) else LogD.encode(decodedCliqueBelief(vars))

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): Factor = cliqueBelief(Array(vi))
}

/** Trait that is implemented by inference algorithms that can compute the most probable explanation, most probable
  * assignment, maximum aposteriori assignment.
  */
trait MPEI { self: Inferer =>
  /** @return the most probably variable assignment. */
  def mpe: Map[Var,Val]
}

class Result(mpi: MargParI) extends MargParI {
  override val problem: Problem = mpi.problem
  def lookupFromMPI(x: MargParI): Array[Array[Double]] = (0 until x.problem.variables.size).map(v => x.variableBelief(v).values)(collection.breakOut)
  val marginals: Array[Array[Double]] = lookupFromMPI(mpi)
  override val logZ = mpi.logZ
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): Factor = Factor(Array(vi),marginals(vi))
  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(logZ)

  override def hashCode(): Int = (problem,logZ,marginals.deep).hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case r: Result => r.logZ == this.logZ && r.problem == this.problem && this.marginals.deep == r.marginals.deep
    case _ => false
  }
}