package vultura.fastfactors.algorithms

import scala.util.Random
import vultura.fastfactors.{Problem, LogD, FastFactor}
import vultura.util.seq2randomSeq
import scala.collection.mutable

/**
 * Conditioned Belief Propagation.
 */
class CBP(val problem: Problem,
          val leafSelection: (Map[Map[Int,Int],BeliefPropagation], Random) => Map[Int,Int],
          val variableSelection: (BeliefPropagation, Random) => Int,
          val clampMethod: CBP.CLAMP_METHOD.Value = CBP.CLAMP_METHOD.CLAMP,
          val bpMaxiter: Int = 1000,
          val bpTol: Double = 1e-10,
          private val random: Random = new Random(0L)) extends InfAlg with Iterator[InfAlg] {
  implicit val (logger, formatter, appender) = CBP.allLog

  val Problem(factors,domains,ring) = problem
  def getProblem: Problem = problem

  var queue: Map[Map[Int,Int],BeliefPropagation] = _
  var iterations: Int = _

  init()

  def init(){
    queue = Map(Map[Int,Int]() -> constructBP(Map()))
    iterations = 0
  }

  def run(maxIter: Int){
    var steps = 0
    while(steps < maxIter){
      val selectAssignment =  leafSelection(queue,random)
      logger.finer(f"CBP refining assignment $selectAssignment")
      val selectVar: Int = variableSelection(queue(selectAssignment),random)
      val newAssignments: IndexedSeq[Map[Int, Int]] =
        for(x <- 0 until domains(selectVar)) yield selectAssignment + (selectVar -> x)
      queue = queue - selectAssignment ++ newAssignments.map(a => a -> constructBP(a))
      steps += 1
//      logger.info("size of queue is " + queue.size)
    }
    iterations += steps
    beliefCache.clear()
  }


  def hasNext: Boolean = true
  var firstRun = true
  def next(): InfAlg = {
    if(!firstRun){
      run(1)
    }
    firstRun = false
    this
  }

  def constructBP(assignment: Map[Int,Int]): BeliefPropagation = {
    import CBP.CLAMP_METHOD._
    val clampFactor: IndexedSeq[FastFactor] => IndexedSeq[FastFactor] = clampMethod match {
      case CONDITION => _.map(_.condition(assignment,domains))
      case CONDITION_SIMPLIFY => _.map(_.condition(assignment,domains).simplify(domains))
      case CLAMP => _ ++ assignment.map{case (variable,value) => FastFactor(Array(variable),Array.fill(domains(variable))(ring.zero).updated(value,ring.one))}
    }
    val conditionedProblem: Problem = problem.copy(factors = clampFactor(problem.factors))
    val bp = new BeliefPropagation(conditionedProblem,random,bpTol,bpMaxiter)
    if(!bp.converged)
      logger.fine(f"bp run did not converge for assignment $assignment")
    bp
  }
  def logZ: Double = {
    val conditionedZs: Array[Double] = queue.map(_._2.logZ)(collection.breakOut)
    LogD.sumA(conditionedZs)
  }
  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double = ring.sumA(queue.map(_._2.Z)(collection.breakOut))

  private val beliefCache = new mutable.HashMap[Int,FastFactor]

  def variableBelief(vi: Int): FastFactor = beliefCache.getOrElseUpdate(vi,queue.map{ case (assignment,bp) =>
    //if the variable is conditioned, construct a factor f with f(v) = bp.Z for v == assigned value and 0 else
    val z = bp.Z
    val range: Range = 0 until domains(vi)
    assignment.get(vi)
      .map(xi => FastFactor(Array(vi), range.map(yi => if (yi == xi) ring.one else ring.zero)(collection.breakOut)))
      .getOrElse(bp.variableBelief(vi).map(ring.prod(_,z)))
  }.reduce[FastFactor]{ case (f1,f2) => FastFactor(f1.variables,f1.values.zip(f2.values).map(t => ring.sum(t._1,t._2)))}.normalize(problem.ring))

  def iteration: Int = iterations
}

object CBP {
  implicit val allLog@(logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

  object CLAMP_METHOD extends Enumeration {
    val CLAMP, CONDITION, CONDITION_SIMPLIFY = Value
  }

  /** Expand evenly. */
  def leafSelectionDepth(leafs: Map[Map[Int,Int], BeliefPropagation], random: Random): Map[Int,Int] =
    vultura.util.maxByMultiple(leafs.keys.toSeq)(ass => -ass.size).pickRandom(random)
  def leafSelectionRandom(leafs: Map[Map[Int,Int],BeliefPropagation], random: Random): Map[Int,Int] = leafs.keys.pickRandom(random)
  def leafSelectionOnlyZ(leafs: Map[Map[Int,Int],BeliefPropagation], random: Random): Map[Int,Int] = leafs.maxBy(_._2.logZ)._1
  def leafSelectionSlowestSettler(leafs: Map[Map[Int,Int],BeliefPropagation], random: Random): Map[Int,Int] =
    vultura.util.maxByMultiple(leafs.toSeq)(_._2.messages.map(_._2.lastUpdate).max).pickRandom(random)._1

  def variableSelectionHighDegree(bp: BeliefPropagation, random: Random): Int =
    vultura.util.maxByMultiple(bp.problem.variables.toSeq)(bp.problem.degreeOfVariable).pickRandom(random)
  def variableSelectionRandom(bp: BeliefPropagation, random: Random): Int = bp.problem.variables.pickRandom(random)
  def variableSelectionSlowestSettler(bp: BeliefPropagation, random: Random): Int = {
    vultura.util.maxByMultiple(bp.messages.toSeq)(_._2.lastUpdate).flatMap(_._2.factor.variables).pickRandom(random)
  }
}

case class CBPConfig(leafSelection: (Map[Map[Int,Int],BeliefPropagation], Random) => Map[Int,Int] = CBP.leafSelectionRandom,
                     variableSelection: (BeliefPropagation, Random) => Int = CBP.variableSelectionRandom,
                     clampMethod: CBP.CLAMP_METHOD.Value = CBP.CLAMP_METHOD.CLAMP,
                     bpMaxiter: Int = 1000,
                     bpTol: Double = 1e-10,
                     seed: Long = 0L) {
  def iterator(p: Problem): Iterator[InfAlg] = new CBP(p,leafSelection,variableSelection,clampMethod,bpMaxiter,bpTol,new Random(seed))
}
