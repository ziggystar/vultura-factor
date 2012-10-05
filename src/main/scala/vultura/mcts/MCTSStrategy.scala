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
trait MCTSStrategy[N,S,R] {
  def problem: Problem[N,R]

  //select a node to investigate. None if this is a leaf
  def select(node: N, stats: Map[N,S], random: Random): Option[N]

  /** Update statistics.
    * This is guaranteed to be called from bottom to top. This is to enable changing the returned value, giving upwards.
    */
  def updateNode(node: N, stats: Map[N,S], result: R): (S,R)

  /** Currently used for empty stats when updating extension node, only. Thus **not** used in selection phase. */
  def zeroStats(node: N): S

  def emptyMap = Map[N,S]()
}

trait Problem[N,R]{
  //define the tree
  def rootNode: N
  def successors(node: N): Seq[N]

  //monte-carlo simulation
  def simulate(node: N, random: Random): R
}

object MCTSStrategy {
  def simulateOnce[N,S,R](strat: MCTSStrategy[N,S,R], stats: Map[N,S], random: Random): Map[N,S] = {
    val path = Stream.iterate(Some(strat.problem.rootNode): Option[N])(_.flatMap(strat.select(_,stats,random)))
    //both instantiated and
    val (definedPath, rest) = path.span(_.exists(stats.contains(_)))
    val updatePath = if(rest.head.isDefined) {
      //we've not hit a leaf of the complete tree
      (definedPath :+ rest.head).map(_.get)
    } else {
      //reevaluate? currently yes
      definedPath.map(_.get)
    }

    //the path of nodes including the new extension node
    val extension = updatePath.last
    val result = strat.problem.simulate(extension,random)
    //update and return new stats map
    val newMap = updatePath.reverse.foldLeft((stats,result)){case ((s,r),node) =>
      val (updatedStats,updatedResult) = strat.updateNode(node,s,r)
      (s + (node -> updatedStats), updatedResult)
    }._1
    newMap
  }
}

case class OrderedFactorSearch[T: ({type l[X] = Factor[X,Double]})#l](factor: T, ordering: IndexedSeq[Int])
  extends Problem[List[Int],Double]{
  //reverse lookup (as array for performance reasons)
  private val reverseOrdering: Array[Int] = (0 until ordering.size).map(ordering.zipWithIndex.map(_.swap).toMap).toArray

  //define the tree
  def rootNode: List[Int] = Nil
  def successors(node: List[Int]): Seq[List[Int]] =
    if(domains(factor).size == node.size) Seq() else domains(factor).apply(node.size).map(_ :: node)

  /** Assign the remaining variable uniformly and return the value of the factor. */
  def simulate(node: List[Int], random: Random): Double = {
    val assignment = node.reverse
    val remainingAssignment = ordering.drop(assignment.size).map(domains(factor)).map(_.pickRandom(random))
    //but ordered according to `ordering`
    val completeAssignment = assignment ++ remainingAssignment
    evaluate(factor,variables(factor).map(i => completeAssignment(reverseOrdering(i))))
  }
}

case class UniformSampling[T: ({type l[X] = Factor[X,Double]})#l](problem: OrderedFactorSearch[T])
  extends MCTSStrategy[List[Int],(Int,Double),Double]
{
  def select(node: List[Int], stats: Map[List[Int],(Int,Double)], random: Random): Option[List[Int]] =
    problem.successors(node).pickRandomOpt(random)

  def updateNode(node: List[Int], oldStats: Map[List[Int],(Int,Double)], result: Double): ((Int, Double), Double) = {
    val (trials,sum) = oldStats.get(node).getOrElse(zeroStats(node))
    val normalizedResult = result * (
      if(variables(problem.factor).size == node.size)
        1
      else
        domains(problem.factor).apply(node.size).size)
    ((trials + 1, sum + normalizedResult), normalizedResult)
  }

  def zeroStats(node: List[Int]): (Int, Double) = (0,0)
}

case class ImportanceSampling[T: ({type l[X] = Factor[X,Double]})#l](problem: OrderedFactorSearch[T], explorationFactor: Double = 1d, sdFactor: Double = 1d)
  extends MCTSStrategy[List[Int],(Int,Double,Double),Double]
{
  def select(node: List[Int], stats: Map[List[Int],(Int,Double,Double)], random: Random): Option[List[Int]] = {
    //draw a successor proportional to the mean
    val candidates = problem.successors(node).toIndexedSeq
    drawRandomlyBy(candidates,random)(selectionProbability(_,stats))
  }

  def updateNode(node: List[Int], oldStats: Map[List[Int],(Int,Double,Double)], result: Double): ((Int, Double, Double),Double) = {
    val (trials,sum,sqsum) = oldStats.get(node).getOrElse(zeroStats(node))

    //the chance that we have been selected
    val q = selectionProbability(node,oldStats)
    val normalizedResult = result * q
    ((trials + 1, sum + result, sqsum + result * result), normalizedResult)
  }

  def selectionProbability(node: List[Int], stats: Map[List[Int],(Int,Double,Double)]): Double = {
    def mean(n: List[Int]): Double = stats.get(n).map(s => s._2 / s._1).getOrElse(1d)
    def trials(n: List[Int]) = stats.get(n).map(_._1).getOrElse(1)
    def normSd(n: List[Int]): Double = stats.get(n).map(s => math.sqrt(s._3) / mean(n)).getOrElse(1d)
    def quality(n: List[Int], parent: List[Int]): Double = {
      mean(n) / mean(parent) + explorationFactor * math.sqrt(math.log(trials(parent)) / trials(n)) + sdFactor * normSd(n)
    }
    node match {
      case Nil => 1
      case _ :: parent => {
        val siblings: Seq[List[Int]] = problem.successors(parent)
        val qualitySiblings = siblings.map(n => quality(n, parent))
        quality(node, parent) / qualitySiblings.sum
      }
    }
  }

  def zeroStats(node: List[Int]): (Int, Double,Double) = (0,0,0)
}


object Main extends App {
  val factor = ProductFactor(
    uai.parseUAIMarkovFromFile(new java.io.File(args(0))),
    RingWithZero.sumProduct.multiplication)
  println("exact partition: " + partition(factor,RingWithZero.sumProduct.addition))
  val problem = OrderedFactorSearch(factor,variables(factor))
  val numTrials = 5
  for(
    exploration <- Seq(0.01,0.02,0.05,0.1,0.2);
    numSamples <- Seq(1000,5000,10000,50000)
  ) yield {
    val strategy = ImportanceSampling(problem,exploration)
    val estimates = (1 to numTrials).map(_ => simulate(strategy,numSamples))
    println("%f\t%d\t%g\t%g".format(exploration,numSamples,estimates.mean,math.sqrt(estimates.variance)))
  }

  def simulate[A,C](strategy: MCTSStrategy[A,(Int,Double,Double),C],numSamples: Int): Double = {
    var state = strategy.emptyMap
    val random = new Random()
    for(_ <- 1 to numSamples) {
      state = MCTSStrategy.simulateOnce(strategy,state,random)
    }
    val rootStats = state(strategy.problem.rootNode)
    rootStats._2 / rootStats._1
  }
}