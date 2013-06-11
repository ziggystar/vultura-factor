package vultura.fastfactors

import scala.collection.mutable
import scala.util.Random

/**
 * Belief Propagation on loopy graphs using `FastFactor` operations.
 * User: Thomas Geier
 * Date: 6/10/13
 */
class BeliefPropagation(factors: IndexedSeq[FastFactor], domains: Array[Int], ring: RingZ[Double]) {
  implicit val (logger, formatter, appender) = BeliefPropagation.allLog

  private val cg = BeliefPropagation.createBetheClusterGraph(factors,domains,ring)

  //the messages are always guaranteed to be normalized
  private val messages: mutable.HashMap[(Int,Int),FastFactor] = new mutable.HashMap
  private var randomOrder: IndexedSeq[(Int, Int)] = makeRandomOrder
  private var totalIterations = 0
  private var lastTol = Double.PositiveInfinity
  private var didConverge = false

  init()

  // ---------- initialisation stuff -------------------
  private def initializeMessages(): Unit = cg.sepsets.foreach {
    case (edge, vars) => messages(edge) = FastFactor.maxEntropy(vars, domains, ring)
  }
  protected def makeRandomOrder: IndexedSeq[(Int, Int)] = Random.shuffle(cg.sepsets.keys.toIndexedSeq)

  def init() {
    initializeMessages()
    randomOrder = makeRandomOrder
    totalIterations = 0
    lastTol = Double.PositiveInfinity
  }
  //----------- initialisation stuff ends here ---------

  def iterations = totalIterations
  def lastDelta = lastTol
  def converged = didConverge

  /** Computes the standard update:
    * $$\delta'_{i-j} \propto \Sum_{C_i - S_{i,j}} \Psi_i \cdot \Prod_{k\in(N_i - \{i\}} \delta_{k - j}$$.
    *
    * @return The delta in normal domain of the update. */
  private def updateMessage(edge: (Int,Int)): Double = {
    val (from,to) = edge
    //multiply all incoming messages for `from` without that coming from `to`; also multiply 'from's factor
    val newFactor = FastFactor.multiplyRetain(ring)(domains)(
      factors = cg.neighbours(from)
        .filterNot(_ == to)
        .map(fromNeighbour => messages((fromNeighbour,from))) :+ cg.clusterFactors(from),
      cg.sepsets(edge)
    ).normalize(ring)
    //mutate message
    //TODO use sumProduct message directly and write to existing array?
    def computeDelta(as: Array[Double], bs: Array[Double]): Double = {
      var i = 0
      var max = Double.NegativeInfinity
      while(i < as.length){
        val newDelta: Double = math.abs(as(i) - bs(i))
        if(newDelta > max)
          max = newDelta
        i += 1
      }
      max
    }
    val delta = computeDelta(messages(edge).values,newFactor.values)
    messages(edge) = newFactor
    delta
  }

  def run(maxiter: Int = 1000, tol: Double = 1e-10) {
    var iterations = 0
    var maxresidual = Double.PositiveInfinity
    while(iterations <= maxiter && maxresidual > tol){
      maxresidual = randomOrder.map(updateMessage).max
      logger.finer(f"BP residual of last iteration: $maxresidual")
      iterations += 1
    }
    didConverge = maxresidual <= tol
    lastTol = maxresidual
    totalIterations += iterations
  }

  def clusterBelief(ci: Int): FastFactor = FastFactor.multiplyRetain(ring)(domains)(
    factors = cg.neighbours(ci).map(from => messages((from,ci))) :+ cg.clusterFactors(ci),
    cg.clusterFactors(ci).variables).normalize(ring)


  def logZ: Double = {
    def expectation(p: Array[Double], f: Array[Double]) = p.zip(f).map(t => if(t._1 == 0) 0 else t._1 * t._2).sum
    def entropy(ps: Array[Double]) = -(for (p <- ps) yield (if(p == 0) 0 else p * math.log(p))).sum

    val clusterExpectation = cg.clusterFactors.zipWithIndex
      .map{case (f, cI) => expectation(ring.decode(clusterBelief(cI).values),ring.decode(f.values).map(math.log))}

    val clusterEntropies = cg.clusterFactors.zipWithIndex
      .map{case (f, cI) => entropy(ring.decode(clusterBelief(cI).values))}

    val variableClusterIndices: Range = factors.size until cg.clusterFactors.size
    val variableEntropies = variableClusterIndices
      .map{vci => cg.neighbours(vci).size * entropy(ring.decode(clusterBelief(vci).values)) }

    clusterExpectation.sum + clusterEntropies.sum - variableEntropies.sum
  }
}


object BeliefPropagation{
  implicit val allLog@(logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

  def createBetheClusterGraph(factors: IndexedSeq[FastFactor],domains: Array[Int], ring: RingZ[Double]): ClusterGraph  = {
    //we append a cluster for each variable after the factor clusters
    val variables = factors.flatMap(_.variables).toSet.toArray
    val variableShift: Int = factors.size //shift a variable by this number to get its cluster index
    val clusterOfVariable: Map[Int,Int] = variables.zipWithIndex.map{case (v,ci) => v -> (ci + variableShift)}(collection.breakOut)
    //variables here are not shifted
    val vars2factors: Map[Int,Array[Int]] = factors.zipWithIndex
      .flatMap{case (f,fi) => f.variables.map(v => v -> fi)}
      .groupBy(_._1)
      .map{case (v,pairings) => v -> (pairings.map(_._2)(collection.breakOut): Array[Int])}

    ClusterGraph(
      clusterFactors = factors ++ variables.map(v => FastFactor(Array[Int](v),Array.fill(domains(v))(ring.one))),
      neighbours = (factors.map(_.variables.map(clusterOfVariable))(collection.breakOut): Array[Array[Int]]) ++ variables.map(vars2factors),
      sepsets = vars2factors.flatMap{case (v,fs) => fs.map(f => (clusterOfVariable(v),f) -> Array(v)) ++ fs.map(f => (f,clusterOfVariable(v)) -> Array(v))}
    )
  }
}

/**
 *
 * @param clusterFactors Contains a factor for each cluster, or `None`, if the neutral factor shall be assumed.
 * @param neighbours `neighbours(i)` contains the indices of all neighbouring clusters.
 * @param sepsets `sepsets((i,j))` contains all variables, that are shared over edge `(i,j)`, for some connected
 *               clusters `i` and `j`.
 */
case class ClusterGraph(clusterFactors: IndexedSeq[FastFactor],
                        neighbours: Array[Array[Int]],
                        sepsets: Map[(Int,Int),Array[Int]]){
  //assert(sepsets.keySet == neighbours.zipWithIndex.map{case (n,i) => n.map(i -> _)}(collection.breakOut))
  def edges: Iterable[(Int,Int)] = for((sinks,srcI) <- neighbours.zipWithIndex; sinkI <- sinks) yield (srcI,sinkI)
}
