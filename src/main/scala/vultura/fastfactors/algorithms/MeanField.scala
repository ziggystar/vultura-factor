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

  private val uncalibrated: mutable.Queue[Int] = mutable.Queue[Int](problem.variables.toSeq.sorted:_*)

  private var iterations: Int = 0

  /** The logarithmic factors of the problem adjacent to each variable. */
  val logFactors: Map[Int,IndexedSeq[FastFactor]] =
    problem.variables.map(v => v -> problem.factorsOfVariable(v).map(f => f.map(math.log)))(collection.breakOut)

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
    val entropies = q.values.map(x => NormalD.entropy(x.values))
    val logExp = problem.factors.map(f => NormalD.expectation(createQFactor(f.variables).values,f.map(math.log).values))

    math.exp(entropies.sum + logExp.sum)
  }

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): FastFactor = q(vi).copy()

  def freeEnergyReport: String = {
    val vEntrops: Seq[(Int, Double)] =
      problem.variables.toSeq.sorted.map(v => v -> NormalD.entropy(q(v).values))
    val logExpects: IndexedSeq[(FastFactor, Double)] =
      problem.factors.map ( f => f -> NormalD.expectation(createQFactor(f.variables).values, f.values.map(math.log)))

    val vEntropStrings = vEntrops.map{case (v,e) => f"$v:\t$e"}.mkString("\n")
    val logExpectationsStrings = logExpects.map{ case (f,le) => f"${f.toBriefString}:\n\t$le"}.mkString("\n")

    val logZ = vEntrops.map(_._2).sum + logExpects.map(_._2).sum
    f"""#Mean-Field free energy break-down
      |log: $logZ normal: ${math.exp(logZ)}
      |##Variable Entropies
      |Sum: ${vEntrops.map(_._2).sum}
      |$vEntropStrings
      |##Factor Log-Expectations
      |Sum: ${logExpects.map(_._2).sum}
      |$logExpectationsStrings
    """.stripMargin
  }
}
