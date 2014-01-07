package vultura.fastfactors.algorithms

import vultura.fastfactors._
import scala.collection.mutable


/**
 * Implements the Mean Field approximation algorithm.
 *
 * @author Thomas Geier
 * @param maxIter The maximum number of updates. One update is recalculating one marginal belief.
 */
class MeanField(problem: Problem, tol: Double = 1e-9, maxIter: Int = 10000) extends InfAlg {

  require(problem.ring == NormalD, "mean field only supports calculation in normal domain")

  private val q: mutable.HashMap[Int,FastFactor] =
    problem.variables.map(v => v -> FastFactor.maxEntropy(Array(v),problem.domains,NormalD))(collection.breakOut)

  private val uncalibrated: mutable.Queue[Int] = mutable.Queue[Int](problem.variables.toSeq:_*)

  private var iterations: Int = 0

  /** The logarithmic factors of the problem adjacent to each variable. */
  val logFactors: Map[Int,IndexedSeq[FastFactor]] =
    problem.variables.map(v => v -> problem.factorsOfVariable(v).map(ff => ff.copy(values=LogD.encode(ff.values))))(collection.breakOut)

  calibrate(tol,maxIter)

  /** @return A FastFactor representing the distribution Q over the given variables. */
  def createQFactor(variables: Array[Int]): FastFactor = FastFactor.multiply(NormalD)(problem.domains)(variables.map(q))

  /**
   * @param v Variable to update the marginals for.
   * @return `true` if an update was made.
   *        True if the variable distribution did change significantly (maxdiff larger than tol).
   */
  def updateVariable(v: Int): Boolean = {
    val newDist: FastFactor = computeMarginal(v)
    val diff = FastFactor.maxDiff(q(v),newDist,NormalD)
    if(diff > tol){
      q.put(v,newDist)
      return true
    }
    false
  }

  /** Calculate the marginal distribution over a given variable, depending on the marginal distribution of
    * the neighbouring variables. */
  def computeMarginal(v: Int): FastFactor = {
    val factorContribs: IndexedSeq[FastFactor] = for {
      logFactor <- logFactors(v)
      qdist = createQFactor(logFactor.variables.filterNot(_ == v))
      expectedLog = FastFactor.multiplyRetain(NormalD)(problem.domains)(Array(logFactor, qdist), Array(v))
    } yield expectedLog
    val newDist: FastFactor = FastFactor.elementWiseSum(factorContribs).map(math.exp).normalize(NormalD)
    newDist
  }

  /** Calibrates the problem with a round-robin schedule according to variable indices.
    * @return `true` if calibration was achieved. */
  def calibrate(cTol: Double, cMaxIter: Int): Boolean = {
    while(iterations < cMaxIter && !uncalibrated.isEmpty){
      val nextVar = uncalibrated.dequeue()
      if(updateVariable(nextVar)){
        uncalibrated.enqueue(problem.neighboursOf(nextVar).filterNot(uncalibrated.contains).toSeq.sorted:_*)
      }
      iterations += 1
    }
    !uncalibrated.isEmpty
  }

  def getProblem: Problem = problem
  def iteration: Int = iterations

  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double = {
    val entropy = q.values.map(x => NormalD.entropy(x.values)).sum
    val logExp = problem.factors
      .map(f => FastFactor.multiplyRetain(NormalD)(problem.domains)(Array(createQFactor(f.variables),f.map(math.log)),Array()))
      .map(_.values(0))
      .sum

    math.exp(entropy + logExp)
  }

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): FastFactor = q(vi).copy()
}
