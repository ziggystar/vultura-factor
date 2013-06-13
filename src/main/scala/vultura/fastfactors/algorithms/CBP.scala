package vultura.fastfactors.algorithms

import scala.util.Random
import vultura.fastfactors.{LogD, FastFactor}
import vultura.util.seq2randomSeq

/**
 * Conditioned Belief Propagation.
 */
class CBP(val problem: Problem,
          random: Random = new Random,
          leafSelection: (Map[Map[Int,Int],BeliefPropagation], Random) => Map[Int,Int],
          variableSelection: (BeliefPropagation, Random) => Int,
          bpMaxiter: Int = 1000,
          bpTol: Double = 1e-7) extends InfAlg {
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
    while(iterations < maxIter){
      val selectAssignment =  leafSelection(queue,random)
      logger.finer(f"CBP refining assignment $selectAssignment")
      val selectVar: Int = variableSelection(queue(selectAssignment),random)
      val newAssignments: IndexedSeq[Map[Int, Int]] =
        for(x <- 0 until domains(selectVar)) yield selectAssignment + (selectVar -> x)
      queue = queue - selectAssignment ++ newAssignments.map(a => a -> constructBP(a))
      iterations += 1
    }
  }

  def constructBP(assignment: Map[Int,Int]): BeliefPropagation = {
    val bp = new BeliefPropagation(problem.copy(factors=problem.factors.map(_.condition(assignment,domains).simplify(domains))),random)
    bp.run(bpMaxiter,bpTol)
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

  def variableBelief(vi: Int): FastFactor = queue.map{ case (assignment,bp) =>
    //if the variable is conditioned, construct a factor f with f(v) = bp.Z for v == assigned value and 0 else
    val z = bp.Z
    val range: Range = 0 until domains(vi)
    assignment.get(vi).map(xi =>
        FastFactor(Array(vi), range.map(yi => if (yi == xi) z else ring.zero)(collection.breakOut))
    )
      .getOrElse(bp.variableBelief(vi).map(ring.prod(_,z)))
  }.reduce[FastFactor]{ case (f1,f2) => FastFactor(f1.variables,f1.values.zip(f2.values).map(t => ring.sum(t._1,t._2)))}


  /** @return marginal distribution of variable in log encoding. */
  def logVariableBelief(vi: Int): FastFactor =
    if(ring == LogD) variableBelief(vi) else LogD.encode(ring.decode(variableBelief(vi)))
}

object CBP {
  implicit val allLog@(logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)
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
