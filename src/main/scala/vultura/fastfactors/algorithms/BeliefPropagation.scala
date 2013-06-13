package vultura.fastfactors.algorithms

import scala.collection.mutable
import scala.util.Random
import vultura.fastfactors.{LogD, RingZ, FastFactor}

/**
 * Belief Propagation on loopy graphs using `FastFactor` operations.
 * User: Thomas Geier
 * Date: 6/10/13
 */
class BeliefPropagation(val problem: Problem, random: Random = new Random)
extends InfAlg {

  val Problem(factors: IndexedSeq[FastFactor], domains: Array[Int], ring: RingZ[Double]) = problem

  def getProblem: Problem = problem

  implicit val (logger, formatter, appender) = BeliefPropagation.allLog

  private val cg = BeliefPropagation.createBetheClusterGraph(factors,domains,ring)
  logger.fine(f"bethe factor graph has ${cg.clusterFactors.size} clusters and ${cg.sepsets.size} edges")
  lazy val singleVariableClusters: Map[Int,Int] = cg.clusterFactors.zipWithIndex
    .collect{case (f,fi) if f.variables.size == 1 => f.variables.head -> fi}(collection.breakOut)

  //the messages are always guaranteed to be normalized
  case class Message(factor: FastFactor, lastUpdate: Long = -1)
  private val messages: mutable.HashMap[(Int,Int),Message] = new mutable.HashMap
  private var randomOrder: IndexedSeq[(Int, Int)] = null
  private var totalIterations = 0
  private var messageUpdates = 0L
  private var lastMaxDelta = Double.PositiveInfinity
  private var didConverge = false

  init()

  // ---------- initialisation stuff -------------------
  private def initializeMessages(): Unit = cg.sepsets.foreach {
    case (edge, vars) => messages(edge) = Message(FastFactor.maxEntropy(vars, domains, ring))
  }
  protected def makeRandomOrder(): IndexedSeq[(Int, Int)] = random.shuffle(cg.sepsets.keys.toIndexedSeq)

  def init() {
    initializeMessages()
    randomOrder = makeRandomOrder()
    totalIterations = 0
    messageUpdates = 0L
    lastMaxDelta = Double.PositiveInfinity
  }
  //----------- initialisation stuff ends here ---------

  def iterations = totalIterations
  def getMessageUpdates = messageUpdates
  def maxDelta = lastMaxDelta
  def converged = didConverge

  /** Computes the standard update:
    * $$\delta'_{i-j} \propto \Sum_{C_i - S_{i,j}} \Psi_i \cdot \Prod_{k\in(N_i - \{i\}} \delta_{k - j}$$.
    *
    * @return The delta in normal domain of the update. */
  private def updateMessage(edge: (Int,Int), tol: Double): Boolean = {
    val (from,to) = edge
    //multiply all incoming messages for `from` without that coming from `to`; also multiply 'from's factor
    val newFactor = FastFactor.multiplyRetain(ring)(domains)(
      factors = cg.neighbours(from)
        .filterNot(_ == to)
        .map(fromNeighbour => messages((fromNeighbour,from)).factor) :+ cg.clusterFactors(from),
      cg.sepsets(edge)
    ).normalize(ring)

    //TODO use sumProduct message directly and write to existing array?
    val oldMessage: Message = messages(edge)
    val delta = maxDiff(oldMessage.factor.values,newFactor.values)
    if(delta > tol){
      messageUpdates += 1
      val message: Message = Message(newFactor, messageUpdates)
      //update message
      messages(edge) = message
      true
    }
    else
      false
  }

  def run(maxiter: Int = 1000, tol: Double = 1e-10) {
    var iterations = 0
    var converged = false
    while(iterations <= maxiter && !converged){
      converged = !randomOrder.map{ case e@(i,j) =>
        val msg = messages(e)
        val needsUpdate =
          (for (influence <- cg.neighbours(i) if influence != j)
          yield messages((influence, i)).lastUpdate >= msg.lastUpdate).exists(identity)

        if( needsUpdate ){
          val r = updateMessage(e,tol)
          logger.finer("update: " + (i,j) + " : " + r)
          r
        }
        else
          false
      }.exists(identity)
      iterations += 1
    }
//    assert(randomOrder.map(updateMessage(_)))
    didConverge = converged
    totalIterations += iterations
  }

  def clusterBelief(ci: Int): FastFactor = FastFactor.multiplyRetain(ring)(domains)(
    factors = cg.neighbours(ci).map(from => messages((from,ci)).factor) :+ cg.clusterFactors(ci),
    cg.clusterFactors(ci).variables).normalize(ring)

  def variableBelief(vi: Int): FastFactor = clusterBelief(singleVariableClusters(vi))
  /** @return marginal distribution of variable in log encoding. */
  def logVariableBelief(vi: Int): FastFactor =
    if(ring == LogD) variableBelief(vi) else LogD.encode(ring.decode(variableBelief(vi)))

  def logZ: Double = {
    def expectation(p: Array[Double], f: Array[Double]) = p.zip(f).map(t => if(t._1 == 0) 0 else t._1 * t._2).sum
    def entropy(ps: Array[Double]) = -(for (p <- ps) yield if (p == 0) 0 else p * math.log(p)).sum

    val clusterExpectation = cg.clusterFactors.zipWithIndex
      .map{case (f, cI) => expectation(ring.decode(clusterBelief(cI).values),ring.decode(f.values).map(math.log))}

    val clusterEntropies = cg.clusterFactors.zipWithIndex
      .map{case (f, cI) => entropy(ring.decode(clusterBelief(cI).values))}

    val variableClusterIndices: Range = factors.size until cg.clusterFactors.size
    val variableEntropies = variableClusterIndices
      .map{vci => cg.neighbours(vci).size * entropy(ring.decode(clusterBelief(vci).values)) }

    clusterExpectation.sum + clusterEntropies.sum - variableEntropies.sum
  }

  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double = if(ring == LogD) logZ else ring.encode(Array(math.exp(logZ)))(0)

  def maxDiff(as: Array[Double], bs: Array[Double]): Double = {
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


