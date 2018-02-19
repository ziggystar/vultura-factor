package vultura.factor.inference.conditioned.lcbp

import java.io.{PrintStream, FileOutputStream}

import org.specs2._
import vultura.factor.inference.JunctionTree
import vultura.factor.{LogD, Benchmarks, FactorMatchers}
import vultura.factor.generators._

import scala.util.Random

class CompareImplementations extends Specification with FactorMatchers {
  val treeSplit0 = FactoredScheme.withMaxDistance(Set(0),1, grid(2,1))
  val treeSplit1 = FactoredScheme.withMaxDistance(Set(5),2, treeK(4, 3))
  val treeSplit2 = FactoredScheme.withMaxDistance(Set(5),2, treeK(10, 3))
  val grid3x3_center = FactoredScheme.withMaxDistance(Set(4),3,grid(3,3))
  val centerConditionedString = FactoredScheme.withMaxDistance(Set(2),1,grid(5,1,2,factorGenerator = expGauss(3)))
  val multiConditionedString = FactoredScheme.withMaxDistance(Set(2,4,6),1,grid(8,1,2,factorGenerator = expGauss(3)))
  val multiConditionedStringSmall = FactoredScheme.withMaxDistance(Set(2,3),0,grid(6,1,2,factorGenerator = expGauss(3), random = new Random(3)))
  val metaLoop1 = FactoredScheme.withMaxDistance((for (x <- 0 to 2; y <- 0 to 2) yield (1 + 3 * x) + (1 + 3 * y) * 9).toSet,2,grid(9,9))

  def asLog(fs: FactoredScheme): FactoredScheme = fs.copy(problem = fs.problem.toRing(LogD))

  override def is =
    "normal ring" ^
      "all exact on singly split tree 0" ! allExactOn(treeSplit0) ^
      "all exact on singly split tree 1" ! allExactOn(treeSplit1) ^
      "all exact on singly split tree 2" ! allExactOn(treeSplit2).pendingUntilFixed ^
      "all exact on center-conditioned 1D" ! allExactOn(centerConditionedString).pendingUntilFixed ^
      "all exact on multi-conditioned 1D small" ! allExactOn(multiConditionedStringSmall).pendingUntilFixed ^
      "all exact on multi-conditioned 1D" ! allExactOn(multiConditionedString).pendingUntilFixed ^
      "bp lcbp agree on singly conditioned 3x3" ! bpAgreeOn(grid3x3_center) ^
      "all agree on meta loop" ! agreeOn(LCBPAlg.all,metaLoop1,1000000).pendingUntilFixed ^ //old implementation takes forever
    p^
    "log ring" ^
      "all exact on singly split tree 0" ! allExactOn(asLog(treeSplit0)) ^
      "all exact on singly split tree 1" ! allExactOn(asLog(treeSplit1)) ^
      "all exact on singly split tree 2" ! allExactOn(asLog(treeSplit2)).pendingUntilFixed ^
      "all exact on center-conditioned 1D" ! allExactOn(asLog(centerConditionedString)).pendingUntilFixed ^
      "all exact on multi-conditioned 1D small" ! allExactOn(asLog(multiConditionedStringSmall)).pendingUntilFixed ^
      "all exact on multi-conditioned 1D" ! allExactOn(asLog(multiConditionedString)).pendingUntilFixed ^
      "bp lcbp agree on singly conditioned 3x3" ! bpAgreeOn(asLog(grid3x3_center)) ^
      "all agree on meta loop" ! agreeOn(LCBPAlg.all,asLog(metaLoop1),1000000).pendingUntilFixed //old implementation takes forever

  def allExactOn(scheme: FactoredScheme) = foreach(LCBPAlg.all.filterNot(_ == LCBP_G_Exact))(exactOn(_,scheme))

  def exactOn(a: LCBPAlg, scheme: FactoredScheme) = {
    val r = a.inferWithScheme(scheme,tol=1e-15)
    val exact = new JunctionTree(scheme.problem)
    (r._2.aka("converged") must beTrue) and
      (r._1.as(_ => a.toString) must haveSameLogZ(exact,1e-9)) and
      (r._1.as(_ => a.toString) must haveSameMarginals(exact,1e-9))
  }

  def bpAgreeOn(s: FactoredScheme) = agreeOn(Seq(LCBP_BP(false),LCBP_BP(true),LCBP_G_BP),s)
  def allAgreeOn(s: FactoredScheme, maxIter: Int = 100000, printTimes: Boolean = false) =
    agreeOn(LCBPAlg.all,s,maxIter, printTimes = printTimes)

  def agreeOn(algs: Seq[LCBPAlg],scheme: FactoredScheme, maxIter: Int = 100000, printTimes: Boolean = false) = {
    def w[A](x: => A, label: String):A = if(printTimes) Benchmarks.printCPUTime(x,label) else x
    val ra = w(algs.head.inferWithScheme(scheme,maxIter = maxIter),algs.head.toString)
    def raConv = ra._2.as(_ => s"${algs.head} converged") must beTrue
    raConv and foreach(algs.tail) { (a: LCBPAlg) =>
      val r = w(a.inferWithScheme(scheme,maxIter = maxIter),a.toString)
      def conv = r._2.as(_ => s"$a converged") must beTrue
      def agree = r._1.as(_ => s"$a with respect to ${algs.head}") must haveSameMarginals(ra._1, 1e-9)
      conv and agree
    }
  }

  def debugOn(label: String)(alg: LCBPAlg,scheme: FactoredScheme, tol: Double =1e-9, maxIter: Int = 100000) = {
    val r = alg.infer(scheme,tol,maxIter)
    val dot = alg.calibrationGraph(r)
    dot.writePDF(s"lcbp-debug-${label}_${alg.toString}.pdf")
    val csv = alg.nodeCSV(r)
    val out = new PrintStream(new FileOutputStream(s"node_values_${label}_${alg.toString}.csv"))
    out.print(csv)
    out.close()
  }
}
