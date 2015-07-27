package vultura.factor.generation

import vultura.factor.{NormalD, Factor, Ring}

import scala.util.Random

/** Generator for potentials. */
trait FactorGenerator[-N]{
  def ring: Ring[Double]
  def generateValues(domains: Array[Int], nodes: IndexedSeq[N], r: Random): Array[Double]
  def generateFactor(domains: Array[Int], nodes: IndexedSeq[N], r: Random): Factor =
    Factor(domains,generateValues(domains,nodes,r))
}

/** The standard Potts model.
  * No energy contribution for non-equal states. Only pairwise potentials.
  * @param weights How to choose the interaction weights. */
case class StandardPottsInteraction[N](weights: Generator[Double], temperature: Double)
  extends FactorGenerator[Any]{

  require(temperature > 0d, "temperature must be larger than zero")

  val ring: Ring[Double] = NormalD
  override def generateValues(domains: Array[Int], nodes: IndexedSeq[Any], r: Random): Array[Double] = ???
}