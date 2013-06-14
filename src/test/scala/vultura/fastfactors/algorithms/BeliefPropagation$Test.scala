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
class BeliefPropagation$Test extends Specification {
  def BpLogZ(factors: IndexedSeq[FastFactor], ring: RingZ[Double], domains: Array[Int]): Double = {
    val bp = new BeliefPropagation(Problem(factors,domains,ring))
    bp.run(10000,1e-10)
    bp.logZ
  }


  val smallTreeProblem1 = generators.treeK(3,2,2,generators.expGauss(1),new Random(1))
  val treeProblem1 = generators.treeK(15,3,16,generators.expGauss(1),new Random(1))
  val treeProblem2 = generators.treeK(8,5,2,generators.expGauss(1),new Random(1))

  def haveSameStructureAs(ref: FastFactor): Matcher[FastFactor] = new Matcher[FastFactor]{
    def apply[S <: FastFactor](t: Expectable[S]): MatchResult[S] = result(
      ref.variables.deep == t.value.variables.deep && ref.values.size == t.value.values.size,
      "has same structure as " + ref,
      "differs in structure from " + ref,
      t
    )
  }
  def haveCloseValuessAs(ref: FastFactor, tol: Double): Matcher[FastFactor] = new Matcher[FastFactor]{
    def apply[S <: FastFactor](t: Expectable[S]): MatchResult[S] = result(
      FastFactor.maxDiff(t.value,ref) < tol,
      "has close marginals to " + ref,
      "differs in some value by  " + FastFactor.maxDiff(t.value,ref),
      t
    )
  }
  def beSimilarTo(ref: FastFactor, tol: Double): Matcher[FastFactor] =
    haveSameStructureAs(ref) and haveCloseValuessAs(ref,tol)

  def bpInfer(problem: Problem): BeliefPropagation = {
    val bp = new BeliefPropagation(problem,new Random(1))
    bp.run(100,1e-10)
    if()
    println(bp.getProblem.uaiString)
    println(bp.getProblem.variables.toSeq.sorted.map(bp.decodedVariableBelief).mkString("\n"))
    bp
  }
  def jtInfer(problem: Problem) = new CalibratedJunctionTree(problem)

  val bpSmallTree1 = bpInfer(smallTreeProblem1)
  val jtSmallTree1 = jtInfer(smallTreeProblem1)
  val bpTree1 = bpInfer(treeProblem1)
  val jtTree1 = jtInfer(treeProblem1)
  val bpTree2 = bpInfer(treeProblem2)
  val jtTree2 = jtInfer(treeProblem2)

  def is: Fragments =
  "constructing bethe graph" ^
    "from one factor over single variable" ^
      "must have correct neighbours" !
        (BeliefPropagation.createBetheClusterGraph(IS(FF(AI(0),AD(1,2))),AI(2),NormalD).neighbours.deep === AAI(AI(1),AI(0)).deep) ^
      "must have correct neighbours" !
        (BeliefPropagation.createBetheClusterGraph(IS(FF(AI(0),AD(1,2))),AI(2),NormalD).neighbours.deep === AAI(AI(1),AI(0)).deep) ^
      "must have correct sepsets" !
        (BeliefPropagation.createBetheClusterGraph(IS(FF(AI(0),AD(1,2))),AI(2),NormalD).sepsets.mapValues(_.toSeq) === Map((0,1) -> Seq(0),(1,0) -> Seq(0))) ^
      p^
  p^
  "tests of BP" ^
    "BP on two independent variables" !
      (BpLogZ(IS(FF(AI(0),AD(1,2)),FF(AI(1),AD(3,4))),NormalD,Array(2,2)) must beCloseTo(math.log(21d),1e-8)) ^
    "BP on one var with two factors" !
      (BpLogZ(IS(FF(AI(0),AD(1,2)),FF(AI(0),AD(3,4))),NormalD,Array(2)) must beCloseTo(math.log(11d),1e-8)) ^
  p^
  "tests on generated trees" ^
    "compare marginals" ^
      (bpTree1.decodedVariableBelief(3) must beSimilarTo(jtTree1.decodedVariableBelief(3),1e-5)) ^
      (bpTree1.decodedVariableBelief(5) must beSimilarTo(jtTree1.decodedVariableBelief(5),1e-5)) ^
      (bpTree1.decodedVariableBelief(8) must beSimilarTo(jtTree1.decodedVariableBelief(8),1e-5)) ^
      (bpTree1.decodedVariableBelief(10) must beSimilarTo(jtTree1.decodedVariableBelief(10),1e-5)) ^
      (bpTree2.decodedVariableBelief(3) must beSimilarTo(jtTree2.decodedVariableBelief(3),1e-5)) ^
      (bpTree2.decodedVariableBelief(5) must beSimilarTo(jtTree2.decodedVariableBelief(5),1e-5)) ^
      (bpTree2.decodedVariableBelief(8) must beSimilarTo(jtTree2.decodedVariableBelief(8),1e-5)) ^
      (bpTree2.decodedVariableBelief(10) must beSimilarTo(jtTree2.decodedVariableBelief(10),1e-5)) ^
    p^
    "compare partition function" ^
      (bpSmallTree1.logZ must beCloseTo(jtSmallTree1.logZ,1e-7)) ^
      (bpTree1.logZ must beCloseTo(jtTree1.logZ,1e-7)) ^
      (bpTree2.logZ must beCloseTo(jtTree2.logZ,1e-7))

}
