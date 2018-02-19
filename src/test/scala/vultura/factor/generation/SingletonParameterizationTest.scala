package vultura.factor.generation

import org.specs2.Specification
import org.specs2.matcher.{MatchResult, Matcher}
import org.specs2.specification.core.SpecStructure
import vultura.factor.{LabeledProblem, Problem}

import scala.util.Random
/**
  * Created by thomas on 02.01.17.
  */
class SingletonParameterizationTest extends Specification {
  override def is: SpecStructure =
    s2"""magnetize some problems and see if the result is as expected
         |add singleton potentials to 2x1 grid without singleton potentials ${checkSmallLatticeWO}
         |add singleton potentials to 2x1 grid with singleton potentials ${checkSmallLatticeWith}
         |""".stripMargin

  def checkSmallLatticeWO: MatchResult[Problem] =
    identityMagnetization(smallLattice).problem must containFactors(1, Array(0d,1d,1d,2d))

  def checkSmallLatticeWith: MatchResult[Problem] =
    identityMagnetization(smallLatticeWithSingletons).problem must containFactors(2,Array(0d,1d))

  /** Magnetize with energy 0,1,2,… .*/
  def identityMagnetization[L](lps: LabeledProblemStructure[L]): LabeledProblem[L] =
    SingletonParameterization{(_: L, n: Int) => Generator.only((0 until n).toArray.map(_.toDouble))}.parameterize(lps).generate(new Random(0))

  /** Just a small lattice 1x2. */
  def smallLattice: LabeledProblemStructure[IndexedSeq[Int]] = FixedDomainsSize(2).addDomains(graph.lattice((2,false))).withSeed(0)
  def smallLatticeWithSingletons: LabeledProblemStructure[IndexedSeq[Int]] = smallLattice.addSingletons(_ => true)

  /** Check for the presence of factors with the specified values. */
  def containFactors(count: Int, values: Array[Double]): Matcher[Problem] =
    be_==(count) ^^ ((p:Problem) => p.factors.map(_.values).count(f => f.deep == values.deep))
}
