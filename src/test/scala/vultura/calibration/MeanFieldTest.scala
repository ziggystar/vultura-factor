package vultura.calibration

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import vultura.factor.generation._
import vultura.factor.{FactorMatchers, NormalD, Problem}
import vultura.util.graph.graphviz.Directed

class MeanFieldTest extends Specification with FactorMatchers {

  def unconnected(size: Int): Problem =
    problemGenerator(Generator.only(graph.unconnected(size)), magnetization = Some(IIDValuedParam(Generator.uniform(-1,1)))).withSeed().problem
      .toRing(NormalD)
  def squareGrid(length: Int) =
    problemGenerator(Generator.only(graph.lattice(length -> false, length -> false))).withSeed().problem
      .toRing(NormalD)
  def tree(size: Int) = problemGenerator(graph.randomTree(size)).withSeed().problem.toRing(NormalD)
  def complete(size: Int) = problemGenerator(Generator.only(graph.complete(size))).withSeed().problem.toRing(NormalD)

  "mean field must be exact for unconnected problem" >> {
    "size 1" >> {
      MeanField(unconnected(1)).computationGraph.renderPDF("mf-1")(Directed(identity))
      exactMeanField(unconnected(1))
    }
    "size 2" >> exactMeanField(unconnected(2))
    "size 10" >> exactMeanField(unconnected(10))
  }

  "mean field must give lower bound for log-partition function on general problems" >> {
    "square 5" >> meanFieldIsLowerBound(squareGrid(5))
    "square 2" >> meanFieldIsLowerBound(squareGrid(2))
    "tree 2"   >> meanFieldIsLowerBound(tree(2))
    "tree 50"   >> meanFieldIsLowerBound(tree(50))
    "complete 5" >> meanFieldIsLowerBound(complete(5))
  }

  def exactMeanField(problem: Problem): MatchResult[Any] = {
    val (r, stat) = Calibrator.calibrate(MeanField(problem))
    (stat.isConverged must beTrue) and
      (r must haveExactZ(problem)) and
      (r must haveExactMarginals(problem))
  }
  
  def meanFieldIsLowerBound(problem: Problem): MatchResult[Any] = {
    val (r, stat) = Calibrator.calibrate(MeanField(problem))
    (stat.isConverged must beTrue) and
      (r.logZ must beLessThan(problem.logZ))
  }
}
