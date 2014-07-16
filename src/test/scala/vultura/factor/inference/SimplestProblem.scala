package vultura.factor.inference

import org.specs2.Specification
import vultura.factor._
import org.specs2.specification.Fragments

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/17/13
 */
class SimplestProblem extends Specification {

  def is: Fragments =
  (inferJT(tiny1) must beCloseTo(logZTiny,1e-4)) ^
    (inferVE(tiny1) must beCloseTo(logZTiny,1e-4))

  val tiny1 = Problem.fromUaiString(
    """MARKOV
      |2
      |2 2
      |2
      |1 0
      |2 0 1
      |2 1 2
      |4 3 5 7 11
    """.stripMargin)
  val logZTiny = 3.784189634

  def inferJT(p: Problem): Double = new JunctionTree(p).logZ
//  def inferBP(p: Problem): Double
  def inferVE(p: Problem): Double = math.log(variableElimination(p))
}
