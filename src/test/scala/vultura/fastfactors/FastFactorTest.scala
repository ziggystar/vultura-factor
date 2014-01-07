package vultura.fastfactors

import org.specs2._
import specification.Fragments
import FastFactor._
import Utils._

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 2/13/13
 */
class FastFactorTest extends Specification with FastFactorMatchers {

  def incTest(reg: Array[Int], doms: Array[Int]): (Int, Seq[Int]) = (incrementCounter(reg,doms),reg.toSeq)
  def incTest2(reg: Array[Int], doms: Array[Int]): (Int, Seq[Int]) = (incrementCounter2(reg,doms),reg.toSeq)
  def sp(vars: Array[Int], doms: Array[Int], fvars: Array[Array[Int]], fvals: Array[Array[Double]]): Array[Double] = {
    val result: Array[Double] = new Array[Double](vars.map(doms).product)
    sumProduct(vars,doms,fvars,fvals,NormalD,result)
    result
  }

  def is: Fragments =
    "building lookup tables" ^
    "incrementLookup 1" ! (buildLookup(AI(0,1),AI(2,2),AI(0)) === AI(1,-1,-1)) ^
      "incrementLookup 2" ! (buildLookup(AI(0,1),AI(2,2),AI(1)) === AI(0,1,-1)) ^
      "incrementLookup 3" ! (buildLookup(AI(1,0),AI(2,2),AI(0)) === AI(0,1,-1)) ^
      "incrementLookup 4" ! (buildLookup(AI(0,1),AI(2,2),AI(0,1)) === AI(1,1,-3)) ^
      "incrementLookup 5" ! (buildLookup(AI(0,1,2),AI(1,2,3),AI(1)) === AI(0,1,-1,-1)) ^
    p^
    "increment counter" ^
      incTest(AI(0, 0, 0), AI(2, 2, 2)) === (0, SI(1, 0, 0)) ^
      incTest(AI(0, 0, 0), AI(2, 2, 2)) === (0, SI(1, 0, 0)) ^
      incTest(AI(1, 1, 1), AI(2, 2, 2)) === (3, SI(0, 0, 0)) ^
    p^
    "increment counter 2" ^
      incTest2(AI(0, 0, 0), AI(2, 2, 2)) === (0, SI(1, 0, 0)) ^
      incTest2(AI(0, 0, 0), AI(2, 2, 2)) === (0, SI(1, 0, 0)) ^
      incTest2(AI(1, 1, 1), AI(2, 2, 2)) === (3, SI(0, 0, 0)) ^
    p^
    "sumProduct method" ^
      "sum over single factor" !
        (sp(AI(),AI(2),AAI(AI(0)),AAD(AD(1,2))) === AD(3)) ^
      "sum over single factor" !
        (sp(AI(),AI(2,2),AAI(AI(0,1)),AAD(AD(1,2,3,4))) === AD(10)) ^
      "multiply two factors" !
        (sp(AI(0),AI(2),AAI(AI(0),AI(0)),AAD(AD(1,2),AD(3,4))) === AD(3,8)) ^
    p^
    "conditioning factors" ^
      "condition 2x2 factor" ^
      FF(VARS(0, 1), VALS(1, 2, 3, 4)).condition(Map(0 -> 0), DOMS(2, 2)) === FF(VARS(1), VALS(1, 3)) ^
      FF(VARS(0, 1), VALS(1, 2, 3, 4)).condition(Map(0 -> 1), DOMS(2, 2)) === FF(VARS(1), VALS(2, 4)) ^
      FF(VARS(0, 1), VALS(1, 2, 3, 4)).condition(Map(1 -> 0), DOMS(2, 2)) === FF(VARS(0), VALS(1, 2)) ^
      FF(VARS(0, 1), VALS(1, 2, 3, 4)).condition(Map(1 -> 1), DOMS(2, 2)) === FF(VARS(0), VALS(3, 4)) ^
     p^
     "condition 3x3 factor" ^
      FF(VARS(0, 1), VALS(1, 2, 3, 4, 5, 6, 7, 8, 9)).condition(Map(0 -> 0), DOMS(3, 3)) === FF(VARS(1), VALS(1, 4, 7)) ^
      FF(VARS(0, 1), VALS(1, 2, 3, 4, 5, 6, 7, 8, 9)).condition(Map(0 -> 1), DOMS(3, 3)) === FF(VARS(1), VALS(2, 5, 8)) ^
      FF(VARS(0, 1), VALS(1, 2, 3, 4, 5, 6, 7, 8, 9)).condition(Map(1 -> 1), DOMS(3, 3)) === FF(VARS(0), VALS(4, 5, 6)) ^
     p^
    p^
    "building factors" ^
      "from function, not using all variables" !
        (FastFactor.fromFunction(Array(0),Array(2,2),_ => 1d) === FastFactor(Array(0),Array(1d,1d)))
      "deterministic max entropy factors" ^
      "with empty condition" !
        (FastFactor.deterministicMaxEntropy(Array(0),Map(),Array(2),NormalD) must haveValuesCloseTo(FF(VARS(0), VALS(0.5,0.5)))) ^
      "irrelevant entries in condition shouldn't matter" !
        (FastFactor.deterministicMaxEntropy(Array(0),Map(1 -> 1),Array(2),NormalD) must haveValuesCloseTo(FF(VARS(0), VALS(0.5,0.5)))) ^
      "complete condition, one var" !
        (FastFactor.deterministicMaxEntropy(Array(0),Map(0 -> 0),Array(2),NormalD) must haveValuesCloseTo(FF(VARS(0), VALS(1,0)))) ^
      "complete condition, two vars" !
        (FastFactor.deterministicMaxEntropy(Array(0,1),Map(0 -> 0,1 -> 0),Array(2,2),NormalD) must haveValuesCloseTo(FF(VARS(0,1), VALS(1,0,0,0)))) ^
      "partial condition" !
        (FastFactor.deterministicMaxEntropy(Array(0,1),Map(0 -> 0),Array(2,2),NormalD) must haveValuesCloseTo(FF(VARS(0), VALS(0.5,0,0.5,0))))
}
