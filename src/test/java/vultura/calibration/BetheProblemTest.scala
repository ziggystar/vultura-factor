package vultura.calibration

import org.specs2.mutable.Specification
import vultura.factor.inference.calibration.LBP
import vultura.factor.{FactorMatchers, NormalD, generation}

import scala.util.Random

class BetheProblemTest extends Specification with FactorMatchers {
  import vultura.factor.generation._

  val tree = problemGenerator(generation.graph.randomTree(10),param = IIDValuedParam(Generator.uniform(-3,3))).generate(new Random(0)).problem.toRing(NormalD)
  val p1 = problemGenerator(Generator.only(generation.graph.lattice(6 -> true, 6 -> true))).generate(new Random(0)).problem

  "bethe approximation yields exact result on tree" >> {
//    tree.dotMarkovNetwork.nodeLabeled(_.toString).writePDF("tree.pdf")
    val (result,stats) = BetheProblem.infer(tree,damping = 0)
    println(stats)
//    result must haveExactMarginals()
    println(s"entropy: ${result.entropy} avE: ${result.averageEnergy}")
    result must haveExactZ()
  }

  "compare BetheProblem to old LBP implementation" >> {
    "on tree" >> {
      val (result,stats) = BetheProblem.infer(tree,damping = 0)
      val oldBPResult = LBP.infer(tree)

      (stats.isConverged must beTrue) and
        (result must haveSameLogZ(oldBPResult,1e-9))
    }
  }
}
