package vultura.inference

import org.specs2._
import vultura.factor.generation._
import vultura.factor.{FactorMatchers, LogD, NormalD, Problem}

import scala.util.Random
/**
  * Created by thomas on 27.05.16.
  */
class BeliefPropagationTest extends Specification with FactorMatchers {

  def addZeros(p: Problem, prob: Double = 0.2, r: Random = new Random(0)): Problem = {
    p.map{f =>
      Iterator.continually(f.map(v => if(r.nextDouble() < prob) p.ring.zero else v)).dropWhile(_.values.forall(_ == p.ring.zero)).next()
    }
  }
  override def is =
    testLogVsNormal(pottsGrid(Seq(2 -> false, 2 -> false), 2, Generator.uniform(-1,1)).generate(new Random(0)).problem, "2x2 grid") ^
    testLogVsNormal(pottsGrid(Seq(4 -> false, 4 -> false), 4, Generator.uniform(-1,1)).generate(new Random(0)).problem, "4x4 grid, dom 4") ^
    testLogVsNormal(addZeros(pottsGrid(Seq(2 -> false, 2 -> false), 2, Generator.uniform(-1,1)).generate(new Random(0)).problem), "2x2 grid, with zeros") ^
    testLogVsNormal(addZeros(pottsGrid(Seq(4 -> false, 4 -> false), 4, Generator.uniform(-1,1)).generate(new Random(0)).problem), "4x4 grid, dom 4, with zeros") ^
    testLogVsNormal(addZeros(pottsGrid(Seq(4 -> false, 4 -> false), 2, Generator.uniform(-1,1)).generate(new Random(0)).problem, 0.7), "4x4 grid, dom 2, with more zeros") ^
    testLogVsNormal(addZeros(pottsGrid(Seq(8 -> false, 4 -> false), 4, Generator.uniform(-1,1)).generate(new Random(0)).problem), "8x4 grid, dom 4, with more zeros") ^
    testMaxDiffNaN(pottsGrid(Seq(4 -> false, 4 -> false), 2, Generator.uniform(-1,1).replace(0.3,Generator.only(Double.NegativeInfinity))).generate(new Random(0)).problem, "2x2 grid")


  def testMaxDiffNaN(p: Problem, name: String) = s"maxdiff must not be NaN when using damping on $name" ! {
    val pLog = p.toRing(LogD)
    val (_,conv) = BeliefPropagation.infer(pLog, damping = 0.5)
    conv.maxDiff.isNaN must beFalse
  }

  def testLogVsNormal(p: Problem, name: String, verbose: Boolean = false) =
    s"log vs normal inference on $name" ! {
      val pLog = p.toRing(LogD)
      val pNorm = p.toRing(NormalD)
      val resultLog = BeliefPropagation.infer(pLog)
      val resultNorm = BeliefPropagation.infer(pNorm)

      if(verbose){
        println("log: " + resultLog._2)
        println("nrm: " + resultNorm._2)
        p.variables.foreach{vi =>
          println(s"$vi\tlog\t${resultLog._1.varBelief(vi).values.mkString(",")}")
          println(s"$vi\tnrm\t${resultNorm._1.varBelief(vi).values.mkString(",")}")
        }
      }

      resultLog._1 must haveSameMarginals(resultNorm._1, tol=1e-6)
    }
}
