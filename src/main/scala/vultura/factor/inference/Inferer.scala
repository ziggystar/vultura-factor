package vultura.factor.inference

import vultura.factor._

/** Base-trait for probabilistic inference algorithms. */
trait Inferer {
  def problem: Problem
}

/** Trait that is implemented by inference algorithms that can compute variable marginals. */
trait MarginalI extends Inferer {
  @deprecated("use varBelief")
  def decodedVariableBelief(vi: Int): Factor = varBelief(vi)

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  @deprecated("use encodedVariableBelief")
  def variableBelief(vi: Int): Factor = encodedVarBelief(vi)
  
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def encodedVarBelief(variable: Int): Factor
  /** (Estimated) variable belief, in normal encoding. */
  def varBelief(variable: Int): Factor =
    if(problem.ring != NormalD) problem.ring.decode(encodedVarBelief(variable)) else encodedVarBelief(variable)

  /** @return marginal distribution of variable in log encoding. */
  def logVariableBelief(vi: Int): Factor  =
    if(problem.ring == LogD) encodedVarBelief(vi) else LogD.encode(varBelief(vi))
}

/** Trait that is implemented by inference algorithms that can compute the partition function. */
trait ParFunI extends Inferer {
  /** @return Natural logarithm of partition function. */
  def logZ: Double
  /** @return Partition function in encoding specified by `ring`. */
  @deprecated("use only logZ")
  def Z: Double = math.exp(logZ)
  @deprecated("use only logZ")
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
  def lookupFromMPI(x: MargParI): Array[Array[Double]] = x.problem.variables.indices.map(v => x.encodedVarBelief(v).values)(collection.breakOut)
  val marginals: Array[Array[Double]] = lookupFromMPI(mpi)

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def encodedVarBelief(vi: Var): Factor =  Factor(Array(vi),marginals(vi))
  override val logZ = mpi.logZ
  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(logZ)

  override def hashCode(): Int = (problem,logZ,marginals.deep).hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case r: Result => r.logZ == this.logZ && r.problem == this.problem && this.marginals.deep == r.marginals.deep
    case _ => false
  }
}

trait IterativeResult {
  def isConverged: Boolean
  def iterations: Long
  def maxDiff: Double
}

case class ConvergenceStats(iterations: Long, maxDiff: Double, isConverged: Boolean) {
  def max(other: ConvergenceStats) = ConvergenceStats(
    iterations max other.iterations,
    maxDiff max other.maxDiff,
    isConverged && other.isConverged
  )
}

trait VariationalResult extends MargParI {
  def averageEnergy: Double
  def entropy: Double

  /** @return Natural logarithm of partition function. */
  override def logZ: Double = averageEnergy + entropy
}

trait RegionBeliefs[R] {
  def regions: Set[R]
  def regionBelief(region: R): Factor
  def scopeOfRegion(region: R): Set[Int]
}