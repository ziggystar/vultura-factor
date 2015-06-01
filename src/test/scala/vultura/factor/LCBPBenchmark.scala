package vultura.factor

import vultura.factor.Benchmarks._
import vultura.factor.generators._
import vultura.factor.generators.GridH2
import vultura.factor.generators.lcbp.GridProblem
import vultura.factor.inference.conditioned._
import vultura.factor.inference.conditioned.lcbp._

import scala.util.Random

object LCBPBenchmark {
  case class LCProblem(p: Problem, scheme: GScheme)

  val gpProblem8x8 = GridProblem(8,1,1,1d,0,4)
  val gpProblem6x6 = GridProblem(6,1,1,1d,0,4)

  val hgrid2x2: FactoredScheme = GridH2(2,2,2,2).generateScheme(2,expGauss(2),expGauss(2),expGauss(2),new Random(0))
  val hgrid4x4: FactoredScheme = GridH2(3,3,4,4).generateScheme(2,expGauss(2),expGauss(2),expGauss(2),new Random(0))

  def fromGridProblem(gp: GridProblem): LCProblem = LCProblem(gp.problem,gp.gscheme)

  val problems: Seq[(String,LCProblem)] = Seq(
//    "grid3x3x2" -> LCProblem(grid(3,3,2,expGauss(1))),
//    "grid5x5x2" -> LCProblem(grid(5,5,2,expGauss(1))),
//    "grid6x6x2" -> LCProblem(grid(6,6,2,expGauss(1))),
    "conditioned 6x6" -> fromGridProblem(gpProblem6x6),
    "conditioned 8x8" -> fromGridProblem(gpProblem8x8)
  )

  def runAllLcbps(): Unit = {
    LCBPAlg.all.filterNot(_ == OldLCBP).foreach{alg =>
      benchmark(() => alg.infer(hgrid4x4,maxIter = 999999), s"$alg on hgrid4x4", warmup = true, repeats = 3)
//      benchmark(() => alg.infer(hgrid9x9,maxIter = 999999), s"$alg on hgrid 9x9")
    }
  }

  def stress(alg: LCBPAlg, seconds: Int = 60): Unit = {
    val time = System.nanoTime()
    while(System.nanoTime() - time < seconds * 1e9){
      println("loop")
      alg.infer(hgrid4x4)
    }
  }

  def main(args: Array[String]): Unit = {
    runAllLcbps()
//    stress(LCBP_G_F_BP(true), 180)
  }
}
