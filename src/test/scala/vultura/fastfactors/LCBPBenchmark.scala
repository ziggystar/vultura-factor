package vultura.fastfactors

import vultura.fastfactors.generators._
import vultura.fastfactors.algorithms.calibration.BP_Cal
import vultura.fastfactors.algorithms.conditioned.{LScheme, DCon, LCBP, GScheme}
import Benchmarks._
import vultura.fastfactors.algorithms.BeliefPropagation

/**
 * Created by thomas on 3/9/14.
 */
object LCBPBenchmark {
  case class LCProblem(p: Problem, scheme: GScheme = GScheme())

  val gpProblem8x8 = GridProblem(grid(8,8,2,expGauss(1)),8,1,1,1d,0,4)
  val gpProblem6x6 = GridProblem(grid(6,6,2,expGauss(1)),6,1,1,1d,0,4)

  def fromGridProblem(gp: GridProblem): LCProblem = LCProblem(gp.problem,gp.gscheme)
  val problems: Seq[(String,LCProblem)] = Seq(
//    "grid3x3x2" -> LCProblem(grid(3,3,2,expGauss(1))),
//    "grid5x5x2" -> LCProblem(grid(5,5,2,expGauss(1))),
//    "grid6x6x2" -> LCProblem(grid(6,6,2,expGauss(1))),
    "conditioned 6x6" -> fromGridProblem(gpProblem6x6),
    "conditioned 8x8" -> fromGridProblem(gpProblem8x8)
  )

  val algorithm: LCProblem => Unit = p => new LCBP(p.p,p.scheme,1e-7,10000)

  def main(args: Array[String]) {
    for{
      (ps, p) <- problems
    } {
      val n = 5
      val task = times(n)(() => algorithm(p))
      warmup(task)
      benchmark(task,s"$n x " + ps)
    }
  }
}

case class GridProblem(problem: Problem, width: Int, margin: Int, influence: Int, coupling: Double, seed: Long, numConditioned: Int){
  require(numConditioned <= 4)
  require(margin <= width/2, "margin larger than half of width")
  require(influence >= 0)
  require(width > 0)
  require(margin >= 0)
  require(coupling >= 0)
  require(problem.variables.size == width * width)

  val conditionVariables: Seq[(Int,Int)] = {
    val opposite: Int = width - 1 - margin
    Seq((margin, margin), (opposite, margin), (margin, opposite), (opposite, opposite)).take(numConditioned)
  }

  def variable(x: Int, y: Int) = x + width * y
  def influences(cx: Int, cy: Int)(x: Int, y: Int) = math.abs(cx - x) <= influence && math.abs(cy - y) <= influence

  val gscheme: GScheme = GScheme((for{
    x <- 0 until width
    y <- 0 until width
    cvars = conditionVariables.filter{case (cx,cy) => influences(cx,cy)(x,y)}
  } yield variable(x,y) -> DCon(cvars.map{case (cx,cy) => LScheme.split(variable(cx,cy),problem.domains)}:_*)).toMap)

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
