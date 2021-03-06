package vultura.factor.inference

import vultura.factor._
import org.specs2._
import scala.util.Random

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/17/13
 */
class WrongInferenceBug extends Specification {
  val problem: Problem = Problem.fromUaiString(
    """MARKOV
      |4
      |2 2 2 2
      |8
      |1 0
      |1 1
      |1 2
      |2 1 3
      |1 3
      |2 0 2
      |2 0 1
      |2 2 3
      |2 2.2555455014370107 0.9790602988605666
      |2 15.102738699968079 13.073179228428454
      |2 3.0556802766852122 3.2970183080199993
      |4 19.17722925005835 0.006407816047633614 0.9213910051128904 1.4130310932427388
      |2 1.2078444308933316 16.85049432127704
      |4 11.107262695120125 0.06689451754188958 514.277175102916 9.887905121081989
      |4 0.3102114478630196 0.14512404211789576 1.1704434821891385 4.77801998109333
      |4 0.08442421524338531 2.186177493640423 0.25692700349463465 67.31897511736409""".stripMargin
  )
  val result = 15.9299

  def is =
    s2"" ^
    (JunctionTree.logZ(problem) must beCloseTo(result,1e-3)) ^
    (math.log(vultura.factor.variableElimination(problem)) must beCloseTo(result,1e-3)) ^
    "bp must infer correct result" ! (new BeliefPropagation(problem,new Random(0),1e-10,100).logZ must beCloseTo(result,1e-3))
}
