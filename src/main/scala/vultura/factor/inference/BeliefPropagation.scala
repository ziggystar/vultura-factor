package vultura.factor.inference

import gnu.trove.map.hash.TLongObjectHashMap
import vultura.factor._

import scala.collection.mutable
import scala.util.Random

/**
 * Belief Propagation on loopy graphs.
 * Direct implementation not using a calibration framework, or precomputed [[vultura.factor.SumProductTask]].
 *
 * @param damping Damping/smoothing retains a portion of the old message value on update. This can sometimes lead to
 *                convergence, where undamped BP fails. `damping` is the ratio of old message value to retain.
 *                This value has to be within [0,1[.
 */
class BeliefPropagation(val problem: Problem, random: Random = new Random, tol: Double = 1e-7, runInitially: Int = 0, damping: Double = 0d)
extends MargParI with JointMargI with Iterator[MargParI] {
  require(damping >= 0 && damping < 1, "damping factor has to be in [0,1[")

  val Problem(factors: IndexedSeq[Factor], domains: Array[Int], ring: Ring[Double]) = problem

  val cg = BeliefPropagation.createBetheClusterGraph(problem)
  lazy val singleVariableClusters: Map[Int,Int] = cg.clusterFactors.zipWithIndex
    .collect{case (f,fi) if f.variables.size == 1 => f.variables.head -> fi}(collection.breakOut)

  //the messages are always guaranteed to be normalized
  class Message(val factor: Factor, var lastUpdate: Long = -1){
    override def toString: String = f"${factor.toBriefString} ($lastUpdate)"
  }

  private val messageMap: TLongObjectHashMap[Message] = new TLongObjectHashMap[Message](cg.sepsets.size * 2)
  @inline
  private def edgeKey(from: Int, to: Int) = (from.asInstanceOf[Long] << 32) | to
  @inline
  def lookUpMessage(fromClusterIdx: Int, toClusterIdx: Int): Message = messageMap.get(edgeKey(fromClusterIdx,toClusterIdx))

  def allMessages: Iterable[Message] = cg.sepsets.keys.map(e => lookUpMessage(e._1,e._2))
  private var randomOrder: IndexedSeq[(Int, Int)] = _
  private var totalIterations = 0
  private var messageUpdates = 0L
  private var lastMaxDelta = Double.PositiveInfinity
  private var didConverge = false

  //--------------- caches
  private val clusterBeliefCache: mutable.HashMap[Int, Factor] = new mutable.HashMap[Int,Factor]
  private def invalidateCaches(){
    clusterBeliefCache.clear()
    logZCache = None
  }
  //--------------- caches end
  init()

  // ---------- initialisation stuff -------------------
  private def initializeMessages(): Unit = cg.sepsets.foreach {
    case ((from,to), vars) => messageMap.put(edgeKey(from,to), new Message(Factor.maxEntropy(vars, domains, ring)))
  }

  protected def makeRandomOrder(): IndexedSeq[(Int, Int)] = random.shuffle(cg.sepsets.keys.toIndexedSeq)

  def init() {
    initializeMessages()
    randomOrder = makeRandomOrder()
    totalIterations = 0
    messageUpdates = 0L
    lastMaxDelta = Double.PositiveInfinity
    run(runInitially)
  }
  //----------- initialisation stuff ends here ---------

  def iterations = totalIterations
  def getMessageUpdates = messageUpdates
  def maxDelta = lastMaxDelta
  def converged = didConverge

  def lastUpdate(variable: Int): Long = {
    val clusterIndex: Int = singleVariableClusters(variable)
    cg.neighbours(clusterIndex)
      .flatMap(n => Seq(lookUpMessage(clusterIndex,n).lastUpdate, lookUpMessage(n,clusterIndex).lastUpdate))
      .max
  }

  /** max over edge(min edge[->], edge[<-]) */
  def lastUpdate2(variable: Int): Long = {
    val clusterIndex: Int = singleVariableClusters(variable)
    cg.neighbours(clusterIndex)
      .map(n => math.min(lookUpMessage(clusterIndex,n).lastUpdate, lookUpMessage(n,clusterIndex).lastUpdate))
      .max
  }

  /** min over max(incoming),max(outgoing) */
  def lastUpdate3(variable: Int): Long = {
    val clusterIndex: Int = singleVariableClusters(variable)
    val incoming = cg.neighbours(clusterIndex).map(n => lookUpMessage(n,clusterIndex).lastUpdate).max
    val outgoing = cg.neighbours(clusterIndex).map(n => lookUpMessage(clusterIndex,n).lastUpdate).max
    math.min(incoming,outgoing)
  }

  /** Computes the standard update:
    * $$\delta'_{i-j} \propto \Sum_{C_i - S_{i,j}} \Psi_i \cdot \Prod_{k\in(N_i - \{i\}} \delta_{k - j}$$.
    *
    * @return The delta in normal domain of the update. */
   private def updateMessage(edge: (Int,Int), tol: Double): Boolean = {
    val (from,to) = edge
    val oldMessage: Message = lookUpMessage(from,to)
    val fromCluster: Factor = cg.clusterFactors(from)

    //multiply all incoming messages for `from` without that coming from `to`; also multiply 'from's factor
    val newValues: Array[Double] =
      if (fromCluster.variables.size == 1) {
        //if the source cluster only contains a single variable, we can compute it without marginalization
        val domainSize = domains(oldMessage.factor.variables(0))
        val factors: Array[Factor] = cg.neighbours(from)
          .filterNot(_ == to)
          .map(fromNeighbour => lookUpMessage(fromNeighbour, from).factor) :+ fromCluster
        val resultArray = new Array[Double](domainSize)
        val numFactors: Int = factors.size
        val product = new Array[Double](numFactors)
        var outer = 0
        var inner = 0
        while (outer < domainSize) {
          while (inner < numFactors) {
            product(inner) = factors(inner).values(outer)
            inner += 1
          }
          resultArray(outer) = ring.prodA(product)
          inner = 0
          outer += 1
        }
        resultArray
      } else {
        val neighbours = cg.neighbours(from)
        //instead of the backward message we need to ignore, we'll put the cluster factor
        val factorsToMultiply = new Array[Factor](neighbours.length)
        var i = 0
        while(i < neighbours.length){
          if(neighbours(i) == to)
            factorsToMultiply(i) = fromCluster
          else
            factorsToMultiply(i) = lookUpMessage(neighbours(i),from).factor
          i += 1
        }
        Factor.multiplyRetain(ring)(domains)(factorsToMultiply, cg.sepsets(edge)).values
      }

    ring.normalizeInplace(newValues)

    //TODO: [performance] use sumProduct message directly and write to existing array?
    val delta = vultura.util.maxDiff(oldMessage.factor.values,newValues)
    if(delta > tol){
      messageUpdates += 1
      //array copy
      var i = 0
      val target: Array[Double] = oldMessage.factor.values
      val newMessageRatio = 1 - damping
      while(i < newValues.length){
        target(i) = newMessageRatio * newValues(i) + damping * target(i)
        i += 1
      }
      oldMessage.lastUpdate = messageUpdates
      true
    }
    else
      false
  }

  def run(maxiter: Int = 1000): Boolean = {
    var iterations = 0
    var converged = false
    val startMessageUpdates = messageUpdates
    while(iterations <= maxiter && !converged){
      converged = true
      var edgeIndex = 0
      while(edgeIndex < randomOrder.length){
        val edge@(i,j) = randomOrder(edgeIndex)
        val lastUpdate: Long = lookUpMessage(i,j).lastUpdate
        var needsUpdate = false
        val incomingNeighbours = cg.neighbours(i)
        var IneighboursIndex = 0
        while(!needsUpdate && IneighboursIndex < incomingNeighbours.length){
          val Ineighbour: Int = incomingNeighbours(IneighboursIndex)
          val foundUpdateReason = (Ineighbour != i) && lookUpMessage(Ineighbour, i).lastUpdate >= lastUpdate
          if(foundUpdateReason)
          needsUpdate = foundUpdateReason
          IneighboursIndex += 1
        }

        if( needsUpdate ){
          val updateConverged: Boolean = updateMessage(edge, tol)
          converged = converged && !updateConverged
        }
        edgeIndex += 1
      }
      iterations += 1
    }

    didConverge = converged
    totalIterations += iterations
    invalidateCaches()
    didConverge
  }

  def hasNext: Boolean = !didConverge

  def next(): MargParI = {
    require(!didConverge)
    run(1)
    this
  }

  def clusterBelief(ci: Int): Factor = clusterBeliefCache.getOrElseUpdate(ci,Factor.multiplyRetain(ring)(domains)(
    factors = cg.neighbours(ci).map(from => lookUpMessage(from,ci).factor) :+ cg.clusterFactors(ci),
    cg.clusterFactors(ci).variables).normalize(ring))

  /** Throws if no clique contains `vars`.
    * @return Normalized belief over given variables in encoding specified by problem ring. */
  override def cliqueBelief(vars: Array[Var]): Factor = {
    require(vars.length > 0)
    val f = vars.tail.foldLeft(problem.factorsOfVariable(vars(0))){case (remaining,v) =>
        remaining.filter(_.variables.contains(v))
    }.minBy(_.variables.size)
    Factor.multiplyRetain(ring)(domains)(Seq(f),vars).normalize(ring)
  }

  var logZCache: Option[Double] = None

  override def logZ: Double = {
    if(logZCache.isDefined)
      logZCache.get
    else{
      def expectation(p: Array[Double], f: Array[Double]): Double = {
        var result = 0d
        var i = 0
        while(i < p.length){
          if(p(i) != 0)
            result += p(i) * f(i)
          i += 1
        }
        require(!result.isNaN)
        result
      }
      def entropy(ps: Array[Double]) = {
        var result = 0d
        var i = 0
        while(i < ps.length){
          if(ps(i) != 0)
            result += ps(i) * math.log(ps(i))
          i += 1
        }
        require(!result.isNaN)
        -result
      }

      val clusterExpectationAndEntropy = cg.clusterFactors.zipWithIndex.map {
        case (clusterFactor, clusterIndex) =>
          expectation(ring.decode(clusterBelief(clusterIndex).values),ring.decode(clusterFactor.values).map(math.log)) +
            entropy(ring.decode(clusterBelief(clusterIndex).values))
      }

      val variableClusterIndices: Range = factors.size until cg.clusterFactors.size
      val variableEntropies = variableClusterIndices
        .map{vci => cg.neighbours(vci).size * entropy(ring.decode(clusterBelief(vci).values)) }

      val clusterExpectationAndEntropySum: Double = clusterExpectationAndEntropy.sum
      val variableEntropySum: Double = variableEntropies.sum
      val result = clusterExpectationAndEntropySum - variableEntropySum
      logZCache = Some(result)
      result
    }
  }

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def encodedVarBelief(variable: Var): Factor = clusterBelief(singleVariableClusters(variable))

  def iteration: Int = iterations

  def graphviz: String = {
    "digraph {\n" +
      cg.clusterFactors.zipWithIndex.map{case (f,i) => f"""$i [label="${f.toBriefString}"]"""}.mkString("\n") + "\n" +
      cg.sepsets.map(ss => ss._1 -> lookUpMessage(ss._1._1,ss._1._2))
        .map{case ((src,sink),msg) => f"""$src -> $sink [label="${msg.factor.toBriefString} : ${msg.lastUpdate}"]"""}
        .mkString("\n") + "\n" +
      "}"
  }
}

object BeliefPropagation{
  def createBetheClusterGraph(problem: Problem): ClusterGraph  = {
    val Problem(factors: IndexedSeq[Factor],domains: Array[Int], ring: Ring[Double]) = problem
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
      clusterFactors = factors ++ variables.map(v => Factor(Array[Int](v),Array.fill(domains(v))(ring.one))),
      neighbours = (factors.map(_.variables.map(clusterOfVariable))(collection.breakOut): Array[Array[Int]]) ++ variables.map(vars2factors),
      sepsets = vars2factors.flatMap{case (v,fs) => fs.map(f => (clusterOfVariable(v),f) -> Array(v)) ++ fs.map(f => (f,clusterOfVariable(v)) -> Array(v))}
    )
  }
}


