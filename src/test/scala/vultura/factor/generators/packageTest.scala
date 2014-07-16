package vultura.factor.generators

import org.specs2._
import scala.util.Random
import org.specs2.specification.Fragments

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/14/13
 */
class packageTest extends Specification with matcher.ParserMatchers {
  import GeneratorParser._
  val parsers = GeneratorParser

  def is: Fragments =
    "topology tests" ^
      topologyTests ^
    p^
    "test parser" ^
      (pRandomK must succeedOn("randomK(2,2,2,2,max-entropy)")) ^
      (pGrid must succeedOn("grid(width=3,height=3,domains=2,expgauss(1))"))



  def topologyTests: Fragments =
    "grid 3x6 has correct number of factors" ! (grid(3,6,2,maxEntropy, new Random(0)).factors.size === (18 + 3*5 + 6 * 2)) ^
    "grid 3x6 has correct number of variables" ! (grid(3,6,2,maxEntropy, new Random(0)).variables.size === 3*6) ^
    "randomK has correct number of variables" ! (randomK(25,100,3,2,maxEntropy,new Random(0)).variables.size == 25) ^
    "randomK has correct number of factors" ! (randomK(25,100,3,2,maxEntropy,new Random(0)).factors.size == 100)

}
