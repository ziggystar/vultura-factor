package vultura.factor.inference.conditioned.lcbp

import org.specs2._
import org.specs2.matcher.Matcher
import org.specs2.specification.Fragments
import vultura.factor.FactorMatchers
import vultura.factor.generators._

import scala.util.Random

/**
 * Created by thomas on 17.02.15.
 */
class CompareImplementations extends Specification with FactorMatchers {
  val treeSplit1 = FactoredScheme.withMaxDistance(Set(5),2, treeK(4, 3, random = new Random(0)))
  val treeSplit2 = FactoredScheme.withMaxDistance(Set(5),2, treeK(10, 3, random = new Random(0)))
  val grid3x3_center = FactoredScheme.withMaxDistance(Set(4),3,grid(3,3,random = new Random(0)))
  val centerConditionedString = FactoredScheme.withMaxDistance(Set(2),1,grid(5,1,2,factorGenerator = expGauss(3),random = new Random(0)))
  val multiConditionedString = FactoredScheme.withMaxDistance(Set(2,4,6),1,grid(8,1,2,factorGenerator = expGauss(3),random = new Random(0)))
  override def is: Fragments =
    "all exact on singly split tree 1" ! allExactOn(treeSplit1) ^
    "all exact on singly split tree 2" ! allExactOn(treeSplit2) ^
    "all exact on center-conditioned string" ! allExactOn(centerConditionedString) ^
    "all exact on multi-conditioned string" ! allExactOn(multiConditionedString) ^
    "exact lcbp agree on singly conditioned 3x3" ! exactAgreeOn(grid3x3_center) ^
    "bp lcbp agree on singly conditioned 3x3" ! bpAgreeOn(grid3x3_center)

  def allExactOn(scheme: FactoredScheme) = foreach(LCBPAlg.all){(a:LCBPAlg) =>
    val r = a.inferWithScheme(scheme,tol=1e-15)
    (r._2.aka("converged") must beTrue) and (r._1 must haveExactMarginals())
  }

  def exactAgreeOn(s: FactoredScheme) = agreeOn(Seq(OldLCBP,LCBP_G_Exact),s)
  def bpAgreeOn(s: FactoredScheme) = agreeOn(Seq(LCBP_BP,LCBP_G_BP),s)
  def allAgreeOn(s: FactoredScheme) = agreeOn(LCBPAlg.all,s)

  def agreeOn(algs: Seq[LCBPAlg],scheme: FactoredScheme) = {
    val ra = algs.head.inferWithScheme(scheme)
    def raConv = ra._2.aka(s"${algs.head} converged") must beTrue
    raConv and foreach(algs.tail) { (a: LCBPAlg) =>
      val r = a.inferWithScheme(scheme)
      def conv = r._2.aka(s"$a converged") must beTrue
      def agree = r._1.aka(a.toString) must haveSameMarginals(ra._1, 1e-9)
      conv and agree
    }
  }

  def debugOn(alg: LCBPAlg) = ???
}
