package vultura.factor.inference

import com.typesafe.scalalogging.LazyLogging
import gnu.trove.TCollections
import gnu.trove.map.hash.TIntObjectHashMap
import vultura.factor._

/** Base-trait for probabilistic inference algorithms. */
trait Inferer {
  def problem: ProblemStructure
  def ring: Ring[Double]
}

/** Trait that is implemented by inference algorithms that can compute variable marginals. */
trait MarginalI extends Inferer with LazyLogging {
  @deprecated("use varBelief", "24.0.0")
  def decodedVariableBelief(vi: Int): Factor = varBelief(vi)

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  @deprecated("use encodedVariableBelief", "24.0.0")
  def variableBelief(vi: Int): Factor = encodedVarBelief(vi)
  
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def encodedVarBelief(variable: Int): Factor
  /** (Estimated) variable belief, in normal encoding. */
  def varBelief(variable: Int): Factor =
    if(ring != NormalD) ring.decode(encodedVarBelief(variable)) else encodedVarBelief(variable)

  /** @return marginal distribution of variable in log encoding. */
  def logVariableBelief(vi: Int): Factor  =
    if(ring == LogD) encodedVarBelief(vi) else LogD.encode(varBelief(vi))

  def showMarginals: String = problem.variables
    .map(vi => s"Var\t$vi:\t" + varBelief(vi).values.map(m => f"$m%02.3f").mkString("|"))
    .mkString("\n")

  /** The KL divergence for a specific variable marginal, when taking this [[MarginalI]] as the exact distribution. */
  def variableKL(estimate: MarginalI, v: Problem#VI): Double = {
    require(this.problem.domains(v) == estimate.problem.domains(v), "variable domains do not match")
    val pr = this.varBelief(v).values
    val pt = estimate.varBelief(v).values
    //The non-negativity of the KL-divergence is ensured by Gibbs inequality
    //The KL-divergence is a sum of positive and negative values; so numerical inaccuracies can result in a negative value
    val result = NormalD.klDivergence(pr,pt)
    if(result < -1e-13) logger.warn(s"a KL-divergence is quite negative: $result")
    math.max(0,result)
  }

  /** The sum of the KL divergence over all marginals, when taking this [[MarginalI]] as the exact distribution. */
  def totalKLDiv(estimate: MarginalI): Double = this.problem.variables.foldLeft(0d)(_ + variableKL(estimate,_))

  /** The total KL divergence divided by the number of nats required to represent one joint state. */
  def normalizedKLDiv(estimate: MarginalI): Double = totalKLDiv(estimate) / problem.domains.foldLeft(0d)(_ + math.log(_))
}

/** Trait that is implemented by inference algorithms that can compute the partition function. */
trait ParFunI extends Inferer {
  /** @return Natural logarithm of partition function. */
  def logZ: Double
  /** @return Partition function in encoding specified by `ring`. */

  @deprecated("use only logZ", "24.0.0")
  def Z: Double = math.exp(logZ)
  @deprecated("use only logZ", "24.0.0")
  def decodedZ: Double = ring.decode(Array(Z))(0)
}

trait MargParI extends MarginalI with ParFunI{
  def toResult = new Result(this)
}

@deprecated("use RegionBeliefs instead", "24.0.0")
trait JointMargI extends MarginalI {
  /** Throws if no clique contains `vars`.
    * @return Normalized belief over given variables in encoding specified by problem ring. */
  def cliqueBelief(vars: Array[Var]): Factor

  def decodedCliqueBelief(vars: Array[Var]): Factor = cliqueBelief(vars).decodeWith(ring)
}

@deprecated("use only RegionBeliefs", "24.0.0")
trait JMIFromRB[R] extends JointMargI {self : RegionBeliefs[R] =>
  /** Throws if no clique contains `vars`.
    * @return Normalized belief over given variables in encoding specified by problem ring. */
  override final def cliqueBelief(vars: Array[Var]): Factor = {
    val varSet = vars.toSet
    val rb = regionBelief(regions
      .filter(r => varSet subsetOf scopeOfRegion(r))
      .minBy(scopeOfRegion(_).size))
    //marginalize belief to query variables
    Factor.multiplyRetain(NormalD)(problem.domains)(Seq(rb),vars)
      .encodeWith(ring)
  }
}

/** This class does not retain a reference to the argument `mpi`, and can thus be used to simply copy the marginals from
  * a result object to reduce heap usage.
  */
class Result(mpi: MargParI) extends MargParI {
  override val problem: ProblemStructure = mpi.problem
  override val ring: Ring[Double] = mpi.ring

  def lookupFromMPI(x: MargParI): Array[Array[Double]] = x.problem.variables.indices.map(v => x.encodedVarBelief(v).values)(collection.breakOut)
  val marginals: Array[Array[Double]] = lookupFromMPI(mpi)

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def encodedVarBelief(vi: Var): Factor =  Factor(Array(vi),marginals(vi))
  override val logZ: Double = mpi.logZ
  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(logZ)

  override def hashCode(): Int = (problem,logZ,marginals.deep).hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case r: Result => r.logZ == this.logZ && r.problem == this.problem && this.marginals.deep == r.marginals.deep
    case _ => false
  }
}

trait VariationalResult extends MargParI { outer =>
  def averageEnergy: Double
  def entropy: Double

  /** @return Natural logarithm of partition function. */
  final lazy val logZ: Double = averageEnergy + entropy

  def copyValues: VariationalResult = new VariationalResult {
    val mpiResult: Result = outer.toResult
    /** @return marginal distribution of variable in encoding specified by `ring`. */
    override def encodedVarBelief(variable: Val): Factor = mpiResult.encodedVarBelief(variable)
    override val averageEnergy: Double = outer.averageEnergy
    override val entropy: Double = outer.entropy
    override val ring: Ring[Double] = outer.ring
    override val problem: ProblemStructure = outer.problem
  }
}

trait RegionBeliefs[R] extends MarginalI {
  def regions: Set[R]

  /** Belief over the variables of a given region.
    * Normal encoding.
    */
  def regionBelief(region: R): Factor
  def scopeOfRegion(region: R): Set[Int]
}

/** Mixin to compute variable marginals from region marginals. Might be inefficient. */
trait VarBeliefFromRegionBelief[R] extends MarginalI {self: RegionBeliefs[R] =>
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def encodedVarBelief(variable: Var): Factor = varBelief(variable).encodeWith(ring)

  def selectRegionForVariableBelief(variable: Var): R =
    regions.filter(scopeOfRegion(_).contains(variable)).minBy(scopeOfRegion(_).size)

  private val variableBeliefCache = TCollections.synchronizedMap(new TIntObjectHashMap[Factor](problem.numVariables))

  /** (Estimated) variable belief, in normal encoding. */
  override def varBelief(variable: Var): Factor = {
    if (variableBeliefCache.containsKey(variable)) {
      variableBeliefCache.get(variable)
    } else {
      val smallestContainingRegion: R = selectRegionForVariableBelief(variable)
      val rbel = regionBelief(smallestContainingRegion)
      val result = Factor.multiplyRetain(NormalD)(problem.domains)(Seq(rbel),Array(variable))
      variableBeliefCache.put(variable,result)
      result
    }
  }
}