package vultura.fastfactors

import vultura.fastfactors.generators._
import vultura.fastfactors.algorithms.calibration.BP_Cal
import vultura.fastfactors.algorithms.conditioned._
import Benchmarks._
import vultura.fastfactors.algorithms.BeliefPropagation
import scala.util.Random
import vultura.fastfactors.algorithms.conditioned.GScheme

/**
 * Created by thomas on 3/9/14.
 */
object LCBPBenchmark {
  case class LCProblem(p: Problem, scheme: GScheme)

  val gpProblem8x8 = GridProblem(8,1,1,1d,0,4)
  val gpProblem6x6 = GridProblem(6,1,1,1d,0,4)

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
