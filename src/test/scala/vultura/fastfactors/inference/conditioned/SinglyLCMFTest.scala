package vultura.fastfactors.inference.conditioned

import org.specs2.Specification
import org.specs2.specification.Fragments
import scala.util.Random
import vultura.fastfactors._
import vultura.fastfactors.inference.MeanField
import vultura.fastfactors.generators
import org.specs2.matcher.{MatchResult, Expectable, Matcher}

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class SinglyLCMFTest extends Specification with FastFactorMatchers {
  def factoredProblem(size: Int): Problem = generators.factorized(size,2,generators.expGauss(1),new Random(1))

  val singleVarProblem: Problem = factoredProblem(1)
  val fourVarProblem: Problem = factoredProblem(4)
  val connectedTwoVarProblem: Problem = {
    //we strip the single variable factors from this problem
    val p = generators.grid(2,1,2,generators.expGauss(1),new Random(1))
    p.copy(factors=p.factors.filter(_.variables.size > 1))
  }

  val smallGrid: Problem = generators.grid(2,2,2,generators.expGauss(1),new Random(1))
  val mediumGrid: Problem = generators.grid(4,4,4,generators.expGauss(1),new Random(1))

  //create conditioning schemes
  def unconditionedMF(p: Problem): SinglyLCMF = directNeighbours(p,Set())
  def directNeighbours(p: Problem, conditionVars: Set[Int]) =
    new SinglyLCMF(p, SimpleScheme(p,conditionVars.map(cv => (p.neighboursOf(cv) + cv) -> cv)))

  def haveLargerZThanMF: Matcher[SinglyLCMF] = new Matcher[SinglyLCMF]{
    def apply[S <: SinglyLCMF](t: Expectable[S]): MatchResult[S] = {
      val mf: MeanField = new MeanField(t.value.problem)
      result(
        t.value.Z must be_>(mf.Z),
        f"yields larger Z than mean field inference: ${t.value.Z} > ${mf.Z}",
        f"has smaller Z than mean field inference: ${t.value.Z} < ${mf.Z}\n" +
        mf.freeEnergyReport + "\n" +
        t.value.freeEnergyReport,
        t
      )
    }
  }

  def beRelativelyCloseTo(expected: Double, tol: Double = 1e-9): Matcher[Double] = (x: Double) =>
    (math.abs(1- x/expected) < tol, f"is different from $expected by a factor of ${x/expected}")

  def is: Fragments =
    "create initial marginal for unconditioned problem" ! {
      unconditionedMF(singleVarProblem).createInitialMarginal(0,Map()) === FastFactor(Array(0),Array(0.5,0.5))
    } ^
    "inference on unconditioned problems" ^
      "LCMF mean field is exact for factorized problems" !
        (unconditionedMF(fourVarProblem) must haveExactZ()) ^
      "unconditioned LCMF on single variable problem" !
        (unconditionedMF(singleVarProblem) must haveExactZ()) ^
      "yield same result as normal MF on small grid" !
        (unconditionedMF(smallGrid).Z must beRelativelyCloseTo(new MeanField(smallGrid).Z)) ^
      "yield same result as normal MF on medium grid" !
        (unconditionedMF(mediumGrid).Z must beRelativelyCloseTo(new MeanField(mediumGrid).Z)) ^
    p^
    "inference on conditioned problems" ^
      "factored problems" ^
        "singly conditioned single variable problem must yield exact result" !
          (new SinglyLCMF(singleVarProblem,SimpleScheme(singleVarProblem,Set((Set(0),0)))) must haveExactZ()) ^
      "singly conditioned factorized problem must yield exact result" !
          (new SinglyLCMF(fourVarProblem,SimpleScheme(fourVarProblem,Set((Set(0,1),0)))) must haveExactZ()) ^
      "singly conditioned factorized problem must yield exact result" !
          (new SinglyLCMF(fourVarProblem,SimpleScheme(fourVarProblem,Set((Set(0,1,2,3),0)))) must haveExactZ()) ^
      p^
      "connected problems" ^
        "two variable problem must be exact" ! (
          directNeighbours(connectedTwoVarProblem,Set(0)) must
            haveExactZ().updateMessage(x => x + "\n"
              + directNeighbours(connectedTwoVarProblem,Set(0)).verboseDescription)) ^
      "smallGrid, conditioned compared to mean field" !
        (directNeighbours(smallGrid,Set(0)) must haveLargerZThanMF) ^
      "smallGrid, conditioned compared to mean field" !
        (directNeighbours(smallGrid,Set(1)) must haveLargerZThanMF) ^
      "smallGrid, conditioned compared to mean field" !
        (directNeighbours(smallGrid,Set(2)) must haveLargerZThanMF) ^
      "smallGrid, conditioned compared to mean field" !
        (directNeighbours(smallGrid,Set(3)) must haveLargerZThanMF) ^
      "mediumGrid conditioned compared to mean field" !
        (directNeighbours(mediumGrid,Set(11,4)) must haveLargerZThanMF) ^
      "mediumGrid conditioned compared to mean field" !
        (directNeighbours(mediumGrid,Set(0,11)) must haveLargerZThanMF) ^
      "mediumGrid conditioned compared to mean field" !
        (directNeighbours(mediumGrid,Set(2,12)) must haveLargerZThanMF) ^
      "mediumGrid conditioned compared to mean field" !
        (directNeighbours(mediumGrid,Set(8)) must haveLargerZThanMF) ^
      "mediumGrid conditioned compared to mean field" !
        (directNeighbours(mediumGrid,Set(7)) must haveLargerZThanMF)
}
