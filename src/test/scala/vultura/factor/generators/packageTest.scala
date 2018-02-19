package vultura.factor.generators

import org.specs2._

import scala.util.Random

class packageTest extends Specification {
  import GeneratorParser._
  val parsers = GeneratorParser

  def is =
    "topology tests" ^
      topologyTests ^
    p^
    "test parser" ^
      (GeneratorParser.parseAll(pRandomK, "randomK(2,2,2,2,max-entropy)").isEmpty must beFalse)  ^
        (GeneratorParser.parseAll(pGrid, "grid(width=3,height=3,domains=2,expgauss(1))").isEmpty must beFalse)



  def topologyTests =
    "grid 3x6 has correct number of factors" ! (grid(3,6,2,maxEntropy, new Random(0)).factors.size === (18 + 3*5 + 6 * 2)) ^
    "grid 3x6 has correct number of variables" ! (grid(3,6,2,maxEntropy, new Random(0)).variables.size === 3*6) ^
    "randomK has correct number of variables" ! (randomK(25,100,3,2,maxEntropy,new Random(0)).variables.size == 25)
}
