package vultura.fastfactors

import org.specs2._
import specification.Fragments
import FastFactor._

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 2/13/13
 */
class PackageTests extends Specification {
  def AI(is: Int*): Array[Int] = is.toArray
  def SI(is: Int*): Seq[Int] = is.toSeq
  def AD(ds: Double*): Array[Double] = ds.toArray
  def AAI(iss: Array[Int]*): Array[Array[Int]] = iss.toArray
  def AAD(iss: Array[Double]*): Array[Array[Double]] = iss.toArray

  def incTest(reg: Array[Int], doms: Array[Int]): (Int, Seq[Int]) = (incrementCounter(reg,doms),reg.toSeq)
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
      (incTest(AI(0,0,0),AI(2,2,2)) === (0,SI(1,0,0))) ^
      (incTest(AI(0,0,0),AI(2,2,2)) === (0,SI(1,0,0))) ^
      (incTest(AI(1,1,1),AI(2,2,2)) === (3,SI(0,0,0))) ^
    p^
    "sumProduct method" ^
      "sumProduct has to throw when trying to marginalize out not-contained variables" !
        (sumProduct(AI(2),AI(2),AAI(AI(1)),AAD(AD(1.0,1.0)),null,null) must throwA("trying to marginalize out non-existent variable")) ^
      "giving wrongly sized result array should fail" !
        (sumProduct(AI(0,1),AI(2,3,4),AAI(AI(0)),AAD(AD(1.0,1.0)),null,result=new Array[Double](7)) must throwA[AssertionError]) ^
      "sum over single factor" !
        (sp(AI(),AI(2),AAI(AI(0)),AAD(AD(1,2))) === AD(3)) ^
      "sum over single factor" !
        (sp(AI(),AI(2,2),AAI(AI(0,1)),AAD(AD(1,2,3,4))) === AD(10)) ^
      "multiply two factors" !
        (sp(AI(0),AI(2),AAI(AI(0),AI(0)),AAD(AD(1,2),AD(3,4))) === AD(3,8))
}
