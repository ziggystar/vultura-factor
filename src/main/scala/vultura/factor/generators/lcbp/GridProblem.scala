package vultura.factor.generators.lcbp

import vultura.factor.inference.BeliefPropagation
import vultura.factor.inference.conditioned.lcbp.{FactoredScheme, DCon, GScheme, LScheme}
import vultura.factor.{LogD, Problem}

import scala.util.Random

/**
 * Creates a square grid inference problem together with a `GScheme` with up to four variables distributed `margin` away
 * from the corners.
 *
 * @param numConditioned Number of conditioning variables to create.
 * @param influence (Manhatten) Distance the influence of conditioning shall reach.
 * @see vultura.factors.algorithms.conditioned.GScheme
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
case class GridProblem(width: Int, margin: Int, influence: Int, coupling: Double, numConditioned: Int, seed: Long = 0){
  require(numConditioned <= 4)
  require(margin <= width/2, "margin larger than half of width")
  require(influence >= 0)
  require(width > 0)
  require(margin >= 0)
  require(coupling >= 0)

  import vultura.factor.generators._
  val problem: Problem = grid(width,width,2,expGauss(coupling),new Random(seed))

  val conditionVariables: Seq[(Int,Int)] = {
    val opposite: Int = width - 1 - margin
    Seq((margin, margin), (opposite, margin), (margin, opposite), (opposite, opposite)).take(numConditioned)
  }

  def variable(x: Int, y: Int) = x + width * y
  def influences(cx: Int, cy: Int)(x: Int, y: Int) = math.abs(cx - x) <= influence && math.abs(cy - y) <= influence

  val gscheme: GScheme = GScheme(problem.domains, (for{
    x <- 0 until width
    y <- 0 until width
    cvars = conditionVariables.filter{case (cx,cy) => influences(cx,cy)(x,y)}
  } yield variable(x,y) -> DCon(cvars.map{case (cx,cy) => LScheme.split(variable(cx,cy),problem.domains)}:_*)).toMap)

  val fScheme: FactoredScheme = FactoredScheme(problem, (for{
    x <- 0 until width
    y <- 0 until width
    cvars: Set[Int] = conditionVariables.filter{case (cx,cy) => influences(cx,cy)(x,y)}.map(xy => variable(xy._1,xy._2))(collection.breakOut)
  } yield variable(x,y) -> cvars).toMap)

  def cbp(tol: Double, iterations: Int): (Double, Boolean) = {
    val variables: Seq[Int] = conditionVariables.map({case (x, y) => variable(x, y)})

    def assignments(variables: Seq[Int]): Seq[Map[Int, Int]] = {
      if (variables.isEmpty) Seq(Map())
      else for (currentAssignment <- 0 until problem.domains(variables.head); tailAssignment <- assignments(variables.tail)) yield tailAssignment + (variables.head -> currentAssignment)
    }

    val ass = assignments(variables)
    val conditionedProblems: Seq[Problem] = for (assignment <- ass) yield problem.map(_.condition(assignment, problem.domains))
    val propagations: Seq[BeliefPropagation] = conditionedProblems.map(new BeliefPropagation(_, util.Random, tol, iterations))
    (LogD.sumA(propagations.map(_.logZ).toArray), propagations.forall(_.converged))
  }
}
