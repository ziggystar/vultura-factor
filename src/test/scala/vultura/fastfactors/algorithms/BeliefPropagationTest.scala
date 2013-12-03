package vultura.fastfactors.algorithms

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.fastfactors._
import vultura.fastfactors.Utils._
import vultura.fastfactors.Problem
import scala.util.Random
import org.specs2.matcher.{MatchResult, Expectable, Matcher}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/10/13
 */
class BeliefPropagationTest extends Specification with FastFactorSpecs {

  val smallTreeProblem1 = generators.treeK(3,2,2,generators.expGauss(1),new Random(1))
  val treeProblem1 = generators.treeK(15,3,16,generators.expGauss(1),new Random(1))
  val treeProblem2 = generators.treeK(15,3,2,generators.expGauss(1),new Random(1))

  def bpInfer(problem: Problem, seed: Long = 1): BeliefPropagation = new BeliefPropagation(problem,new Random(seed),1e-10,100)
  def jtInfer(problem: Problem) = new CalibratedJunctionTree(problem)

  val testProblem1 = Problem(IS(FF(AI(0),AD(1,2)),FF(AI(0),AD(3,4))),Array(2),NormalD)
  val testProblem1lnZ = math.log(11d)
  val testProblem2 = Problem(IS(FF(AI(0),AD(1,2)),FF(AI(1),AD(3,4))),Array(2,2),NormalD)
  val testProblem2lnZ = math.log(21d)

  val randomProblem = generators.grid(6,6,2,generators.expGauss(3),new Random(0))

  val bpSmallTree1 = bpInfer(smallTreeProblem1)
  val jtSmallTree1 = jtInfer(smallTreeProblem1)
  val bpTree1 = bpInfer(treeProblem1)
  val jtTree1 = jtInfer(treeProblem1)
  val bpTree2 = bpInfer(treeProblem2)
  val jtTree2 = jtInfer(treeProblem2)

  def bpDeterminism(p: Problem): Fragments = {
    val seed = new Random().nextLong()
    "test on seed " + seed !
      ((new BeliefPropagation(p,new Random(seed),1e-10,10).toResult === new BeliefPropagation(p,new Random(seed),1e-10,10).toResult) and
      (new BeliefPropagation(p,new Random(seed),1e-10,10).toResult !== new BeliefPropagation(p,new Random(seed + 1),1e-10,10).toResult))
  }

  def is: Fragments =
  "constructing bethe graph" ^
    "from one factor over single variable" ^
      "must have correct neighbours" !
        (BeliefPropagation.createBetheClusterGraph(Problem(IS(FF(AI(0),AD(1,2))),AI(2),NormalD)).neighbours.deep === AAI(AI(1),AI(0)).deep) ^
      "must have correct neighbours" !
        (BeliefPropagation.createBetheClusterGraph(Problem(IS(FF(AI(0),AD(1,2))),AI(2),NormalD)).neighbours.deep === AAI(AI(1),AI(0)).deep) ^
      "must have correct sepsets" !
        (BeliefPropagation.createBetheClusterGraph(Problem(IS(FF(AI(0),AD(1,2))),AI(2),NormalD)).sepsets.mapValues(_.toSeq) === Map((0,1) -> Seq(0),(1,0) -> Seq(0))) ^
      p^
  p^
  "tests of BP" ^
    "BP on two independent variables" !
      (bpInfer(testProblem2).logZ must beCloseTo(testProblem2lnZ,1e-8)) ^
    "BP on one var with two factors" !
      (bpInfer(testProblem1).logZ must beCloseTo(testProblem1lnZ,1e-8)) ^
  p^
  "tests on generated trees" ^
    "compare marginals" ^
      (bpSmallTree1.decodedVariableBelief(0) must beSimilarTo(jtSmallTree1.decodedVariableBelief(0),1e-5)) ^
      (bpSmallTree1.decodedVariableBelief(1) must beSimilarTo(jtSmallTree1.decodedVariableBelief(1),1e-5)) ^
      (bpTree1.decodedVariableBelief(3) must beSimilarTo(jtTree1.decodedVariableBelief(3),1e-5)) ^
      (bpTree1.decodedVariableBelief(5) must beSimilarTo(jtTree1.decodedVariableBelief(5),1e-5)) ^
      (bpTree1.decodedVariableBelief(8) must beSimilarTo(jtTree1.decodedVariableBelief(8),1e-5)) ^
      (bpTree1.decodedVariableBelief(10) must beSimilarTo(jtTree1.decodedVariableBelief(10),1e-5)) ^
      (bpTree2.decodedVariableBelief(0) must beSimilarTo(jtTree2.decodedVariableBelief(0),1e-5)) ^
      (bpTree2.decodedVariableBelief(3) must beSimilarTo(jtTree2.decodedVariableBelief(3),1e-5)) ^
      (bpTree2.decodedVariableBelief(5) must beSimilarTo(jtTree2.decodedVariableBelief(5),1e-5)) ^
      (bpTree2.decodedVariableBelief(6) must beSimilarTo(jtTree2.decodedVariableBelief(6),1e-5)) ^
    p^
    "compare partition function" ^
      (bpSmallTree1.logZ must beCloseTo(jtSmallTree1.logZ,1e-7)) ^
      (bpTree1.logZ must beCloseTo(jtTree1.logZ,1e-7)) ^
      (bpTree2.logZ must beCloseTo(jtTree2.logZ,1e-7)) ^
    p^
    "running twice with same random seeds must yield same results" ^
      bpDeterminism(generators.grid(8,8,2,generators.expGauss(3),new Random(0))) ^
      bpDeterminism(generators.grid(8,8,2,generators.expGauss(3),new Random(1))) ^
      bpDeterminism(generators.grid(8,8,2,generators.expGauss(3),new Random(2)))

}
