package vultura.fastfactors.generators

import org.specs2.Specification
import org.specs2.specification.Fragments
import scala.util.Random

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/14/13
 */
class package$Test extends Specification {
  def is: Fragments =
    "topology tests" ^
      topologyTests

  def topologyTests: Fragments =
    "grid 3x6 has correct number of factors" ! (grid(3,6,2,maxEntropy, new Random(0)).factors.size === (18 + 3*5 + 6 * 2)) ^
    "grid 3x6 has correct number of variables" ! (grid(3,6,2,maxEntropy, new Random(0)).variables.size === 3*6) ^
    "randomK has correct number of variables" ! (randomK(25,100,3,2,maxEntropy,new Random(0)).variables.size == 25) ^
    "randomK has correct number of factors" ! (randomK(25,100,3,2,maxEntropy,new Random(0)).factors.size == 100)

}
