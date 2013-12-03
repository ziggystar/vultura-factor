package vultura.fastfactors.algorithms

import scala.collection.mutable
import scala.util.Random
import vultura.fastfactors.{Problem, FastFactor}
import vultura.util.seq2randomSeq

/**
 * Conditioned Belief Propagation.
 */
class CBP(val problem: Problem,
          val leafSel: CBP.LEAF_SELECTION.Value,
          val varSel: (BeliefPropagation, Random) => Int,
          val clampMethod: CBP.CLAMP_METHOD.Value = CBP.CLAMP_METHOD.CLAMP,
          val bpMaxiter: Int = 1000,
          val bpTol: Double = 1e-10,
          private val random: Random) extends InfAlg with Iterator[CBP] {
  implicit val (logger, formatter, appender) = CBP.allLog

  val leafSelection: (Map[Map[Int,Int],BeliefPropagation], Random) => Map[Int,Int] = CBP.LEAF_SELECTION.apply(leafSel)
  val variableSelection: (BeliefPropagation, Random) => Int = varSel

  val Problem(factors,domains,ring) = problem
  def getProblem: Problem = problem

  var exactlySolved: Map[Map[Int,Int],InfAlg] = _
  var queue: Map[Map[Int,Int],BeliefPropagation] = _
  var iterations: Int = _

  init()

  def init(){
    exactlySolved = Map()
    queue = Map()
    processPA(Map()) match {
      case Left(bp) => queue = Map(Map[Int,Int]() -> bp)
      case Right(ia) => exactlySolved = Map(Map[Int,Int]() -> ia)
    }
    iterations = 0
  }

  def run(maxIter: Int){
    var steps = 0
    while(steps < maxIter && !queue.isEmpty){
      val selectAssignment =  leafSelection(queue,random)
      logger.finer(f"CBP refining assignment $selectAssignment")
      val selectVar: Int = variableSelection(queue(selectAssignment),random)
      val newAssignments: IndexedSeq[Map[Int, Int]] =
        for(x <- 0 until domains(selectVar)) yield selectAssignment + (selectVar -> x)

      val newLeafs: IndexedSeq[(Map[Int, Int], Either[BeliefPropagation, InfAlg])] = newAssignments.map(a => a -> processPA(a))
      val newBPLeafs: IndexedSeq[(Map[Int, Int], BeliefPropagation)] = newLeafs.collect{case (pa,Left(bp)) => (pa,bp)}
      val newExactLeafs: IndexedSeq[(Map[Int, Int], InfAlg)] = newLeafs.collect{case (pa,Right(ia)) => (pa,ia)}
      exactlySolved = exactlySolved ++ newExactLeafs
      queue = queue - selectAssignment ++ newBPLeafs
      steps += 1
    }
    iterations += steps
    beliefCache.clear()
  }


  def hasNext: Boolean = true
  private var firstRun = true
  def next(): CBP = {
    if(!firstRun){
      run(1)
    }
    firstRun = false
    this
  }

  /** @return right is exact solution. */
  def processPA(assignment: Map[Int,Int]): Either[BeliefPropagation,InfAlg] = {
    import CBP.CLAMP_METHOD._
    val clampFactor: IndexedSeq[FastFactor] => IndexedSeq[FastFactor] = clampMethod match {
      case CONDITION => _.map(_.condition(assignment,domains))
      case CONDITION_SIMPLIFY => _.map(_.condition(assignment,domains).simplify(domains))
      case CLAMP => _ ++ assignment.map{
        case (variable,value) => FastFactor(Array(variable),Array.fill(domains(variable))(ring.zero).updated(value,ring.one))
      }
    }
    val conditionedProblem: Problem = problem.copy(factors = clampFactor(problem.factors))
    if(conditionedProblem.variables.isEmpty)
      Right(new CalibratedJunctionTree(conditionedProblem))
    else
      Left(constructBP(conditionedProblem))
  }

  def constructBP(p: Problem): BeliefPropagation = {
    val seed = random.nextLong()
    val bp = new BeliefPropagation(p, new Random(seed),bpTol,bpMaxiter)
    if(!bp.converged)
      logger.fine(f"bp run did not converge")
    bp
  }

  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double = ring.sumA(conditionedPRs)

  /** @return The entropy of the distribution over the condition. */
  def conditionEntropy: Double = ring.entropy(ring.normalize(conditionedPRs))

  def conditionedPRs: Array[Double] = (queue.map(_._2.Z) ++ exactlySolved.map(_._2.Z))(collection.breakOut)

  private val beliefCache = new mutable.HashMap[Int,FastFactor]

  def variableBelief(vi: Int): FastFactor = beliefCache.getOrElseUpdate(vi,(queue ++ exactlySolved).map{ case (assignment,bp) =>
    //if the variable is conditioned, construct a factor f with f(v) = bp.Z for v == assigned value and 0 else
    val z = bp.Z
    val range: Range = 0 until domains(vi)
    assignment.get(vi)
      .map(xi => FastFactor(Array(vi), range.map(yi => if (yi == xi) z else ring.zero)(collection.breakOut)))
      .getOrElse(bp.variableBelief(vi).map(ring.prod(_,z)))
  }.reduce[FastFactor]{ case (f1,f2) => FastFactor(f1.variables,f1.values.zip(f2.values).map(t => ring.sum(t._1,t._2)))}
    .normalize(problem.ring))

  def iteration: Int = iterations
}

object CBP {
  implicit val allLog@(logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

  trait TypefulEnum[A]{ self: Enumeration =>
    def apply(value: self.Value): A
  }

  object CLAMP_METHOD extends Enumeration {
    val CLAMP, CONDITION_SIMPLIFY, CONDITION = Value
  }

  object VARIABLE_SELECTION extends Enumeration with TypefulEnum[(BeliefPropagation, Random) => Int]{
    val MAX_DEGREE, RANDOM, LAST_UPDATE, BACKDOOR, MIN_ENTROPY, LU_BIDI = Value

    def variableSelectionHighDegree(bp: BeliefPropagation, random: Random): Int =
      vultura.util.maxByMultiple(bp.problem.variables.toSeq)(bp.problem.degreeOfVariable).pickRandom(random)

    def variableSelectionRandom(bp: BeliefPropagation, random: Random): Int = bp.problem.variables.pickRandom(random)

    def variableSelectionSlowestSettler(bp: BeliefPropagation, random: Random): Int =
      vultura.util.maxByMultiple(bp.allMessages.toSeq)(_.lastUpdate).flatMap(_.factor.variables).pickRandom(random)

    def lastUpdateBidir(bp: BeliefPropagation, random: Random): Int ={
      def lastBidirectionalUpdate(v: Int) = {
        val variableCluster: Int = bp.singleVariableClusters(v)
        bp.cg.neighbours(variableCluster).map {
          n =>
            math.min(bp.lookUpMessage(variableCluster, n).lastUpdate, bp.lookUpMessage(n, variableCluster).lastUpdate)
        }.max
      }

      vultura.util.maxByMultiple(bp.getProblem.variables.toSeq)(lastBidirectionalUpdate).pickRandom(random)
    }

    def backdoor(bp: BeliefPropagation, random: Random): Int = {
      val cliques = for {
        tree <- bp.problem.minDegreeJunctionTrees(random)
        (clique,_) <- tree.flatten
      } yield clique
      vultura.util.maxByMultiple(cliques)(_.size).pickRandom(random).pickRandom(random)
    }

    def maxentropy(bp: BeliefPropagation, random: Random): Int = {
      vultura.util.maxByMultiple(bp.problem.variables.toSeq)(
        v => bp.problem.ring.entropy(bp.variableBelief(v).values)
      ).pickRandom(random)
    }

    def apply(v: Value): (BeliefPropagation, Random) => Int = v match {
      case MAX_DEGREE => variableSelectionHighDegree
      case RANDOM => variableSelectionRandom
      case LAST_UPDATE => variableSelectionSlowestSettler
      case LU_BIDI => lastUpdateBidir
      case BACKDOOR => backdoor
      case MIN_ENTROPY => maxentropy
    }
  }

  object LEAF_SELECTION extends Enumeration with TypefulEnum[(Map[Map[Int, Int], BeliefPropagation], Random) => Map[Int, Int]]{
    val MIN_DEPTH,RANDOM,MAX_Z,SLOW_SETTLER = Value

    /** Expand evenly. */
    def leafSelectionDepth(leafs: Map[Map[Int,Int], BeliefPropagation], random: Random): Map[Int,Int] =
      vultura.util.maxByMultiple(leafs.keys.toSeq)(ass => -ass.size).pickRandom(random)
    def leafSelectionRandom(leafs: Map[Map[Int,Int],BeliefPropagation], random: Random): Map[Int,Int] =
      leafs.keys.pickRandom(random)
    def leafSelectionOnlyZ(leafs: Map[Map[Int,Int],BeliefPropagation], random: Random): Map[Int,Int] =
      vultura.util.maxByMultiple(leafs.toSeq)(_._2.logZ).pickRandom(random)._1
    def leafSelectionSlowestSettler(leafs: Map[Map[Int,Int],BeliefPropagation], random: Random): Map[Int,Int] =
      vultura.util.maxByMultiple(leafs.toSeq)(_._2.allMessages.map(_.lastUpdate).max).pickRandom(random)._1

    def apply(v: Value): (Map[Map[Int, Int], BeliefPropagation], Random) => Map[Int, Int] = v match {
      case MIN_DEPTH => leafSelectionDepth
      case RANDOM => leafSelectionRandom
      case MAX_Z => leafSelectionOnlyZ
      case SLOW_SETTLER => leafSelectionSlowestSettler
    }
  }
}

case class CBPConfig(leafSelection: CBP.LEAF_SELECTION.Value = CBP.LEAF_SELECTION.RANDOM,
                     variableSelection: CBP.VARIABLE_SELECTION.Value = CBP.VARIABLE_SELECTION.RANDOM,
                     clampMethod: CBP.CLAMP_METHOD.Value = CBP.CLAMP_METHOD.CLAMP,
                     bpMaxiter: Int = 1000,
                     bpTol: Double = 1e-15) extends AlgConfig {
  def iterator(p: Problem, seed: Long): CBP = new CBP(
    p,
    leafSelection,
    CBP.VARIABLE_SELECTION(variableSelection),
    clampMethod,
    bpMaxiter,
    bpTol,
    new Random(seed))
}
