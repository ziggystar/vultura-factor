package vultura.mcts

import util.Random
import vultura.factors._
import vultura.util._

/**
 * Typeclass for MCTS problems.
 *
 * @tparam N Node type.
 * @tparam R Result type.
 * @tparam S Type of node statistics.
 */
trait MCTS[N,S,R] {
  //define the tree
  def rootNode: N
  def successors(node: N): Seq[N]

  //monte-carlo simulation
  def simulate(node: N, random: Random): R

  //select a node to investigate
  def select(node: N, stats: N => Option[S], random: Random): N

  /** Update statistics.
    * This is guaranteed to be called from bottom to top. This is to enable changing the returned value, giving upwards.
    */
  def updateNode(node: N, stats: S, result: R): (S,R)
}

object MCTS {
  def simulateOnce[N,S,R](problem: MCTS[N,S,R], stats: Map[N,S], random: Random): Map[N,S] = {
    val path = Stream.iterate(problem.rootNode)(n => problem.select(n,stats.get,random))

  }
}

case class AdaptiveIS[T: ({type l[X] = Factor[X,Double]})#l] (factor: T, ordering: IndexedSeq[Int])
  extends MCTS[List[Int],(Int,Double),Double]
{
  //reverse lookup (as array for performance reasons)
  val reverseOrdering: Array[Int] = (0 until ordering.size).map(ordering.zipWithIndex.map(_.swap).toMap).toArray

  def select(node: List[Int], stats: List[Int] => Option[(Int,Double)], random: Random): List[Int] = {
    //draw a successor proportional to the mean
    val candidates = successors(node)
    val mean = 1 / candidates.size.toDouble
    val means: Array[Double] = candidates.map(v => stats(v).map(s => s._2 / s._1).getOrElse(mean))(collection.breakOut)
    candidates(drawRandomlyBy(0 until candidates.size,random)(means).get)
  }

  /** Assign the remaining variable uniformly and return the value of the factor. */
  def simulate(node: List[Int], random: Random): Double = {
    val assignment = node.reverse
    val remainingAssignment = ordering.drop(assignment.size).map(domains(factor)).map(_.pickRandom(random))
    //but ordered according to `ordering`
    val completeAssignment = assignment ++ remainingAssignment
    evaluate(factor,variables(factor).map(i => completeAssignment(reverseOrdering(i))))
  }

  def updateNode(node: List[Int], oldStats: (Int, Double), result: Double): ((Int, Double), Double) = {
    val (trials,sum) = oldStats
    ((trials + 1, sum + result), result)
  }

  //define the tree
  def rootNode: List[Int] = Nil
  def successors(node: List[Int]): Seq[List[Int]] = domains(factor).apply(node.size).map(_ :: node)
}
