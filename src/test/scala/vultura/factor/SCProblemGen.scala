package vultura.factor

import org.scalacheck._
import vultura.factor.inference.{JunctionTree, MarginalI, ParFunI}
import vultura.factor.generators._

import scala.util.Random

/** ScalaCheck problem generators. */
trait SCProblemGen {
  def somePotential: Gen[FactorGenerator] = Gen.oneOf(Seq(expGauss(1),attractive(1),attractive(-1),clause(1)))

  def treeProblem = Gen.sized(n =>
    for{
      nodes <- Gen.choose(0,n)
      cliqueSize <- Gen.choose(2,4)
      domainSize <- Gen.choose(2,3)
      pot <- somePotential
      seed <- Arbitrary.arbitrary[Long]
    } yield vultura.factor.generators.treeK(n, cliqueSize, domainSize, pot, new Random(seed))
  )

  def gridProblem = Gen.sized(n =>
    for{
      width <- Gen.choose(1,n-1)
      height = n - width
      domainSize <- Gen.choose(2,3)
      pot <- somePotential
      seed <- Arbitrary.arbitrary[Long]
    } yield vultura.factor.generators.grid(width,height,domainSize,pot, new Random(seed))
  )
}
