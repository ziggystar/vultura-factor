package vultura.fastfactors

import scala.util.Random

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/14/13
 */
package object generators {
  trait FactorGenerator extends ((Seq[Int],IndexedSeq[Int],Random) => FastFactor) {
    def generate(variables: Array[Int], domains: Array[Int], random: Random): FastFactor
    def apply(v1: Seq[Int], v2: IndexedSeq[Int], v3: Random): FastFactor = generate(v1.toArray.sorted,v2.toArray,v3)
  }

  /** @return a neutral factor. */
  object maxEntropy extends FactorGenerator{
    def generate(variables: Array[Int], domains: Array[Int], random: Random): FastFactor ={
      val entries: Int = variables.map(domains).product
      FastFactor(variables, Array.fill(entries)(1d/entries))
    }
  }

  /** @return a factor that is 1 for a random entry and `weight` for all others. */
  def clause(weight: Double, neutral: Double = 1d): FactorGenerator = new FactorGenerator {
    def generate(variables: Array[Int], domains: Array[Int], random: Random): FastFactor = {
      val entries: Int = variables.map(domains).product
      new FastFactor(variables, Array.fill(entries)(weight).updated(random.nextInt(entries),neutral))
    }
  }

  /** @return a factor that is 1 for all entries except one, which is 0. */
  def deterministicClause = clause(1d,0d)

  def expGauss(sigma: Double = 1d, mean: Double = 0d): FactorGenerator = new FactorGenerator{
    def generate(variables: Array[Int], domains: Array[Int], random: Random): FastFactor =
      FastFactor(variables,Array.fill(variables.map(domains).product)(math.exp(random.nextGaussian() * sigma + mean)))
  }

  def grid(width: Int, height: Int, domainSize: Int, factorGenerator: FactorGenerator, random: Random): Problem = {
    val variables: Map[(Int, Int), Int] = (for (x <- 0 until width; y <- 0 until height) yield (x, y)).zipWithIndex.toMap
    val domains = Array.fill(variables.size)(domainSize)
    val horizontalPairs =
      for(left <- variables.keys if left._1 + 1 < width)
      yield factorGenerator(Seq(left,(left._1 + 1,left._2)).map(variables),domains,random)
    val verticalPairs =
      for(above <- variables.keys if above._2 + 1 < height)
      yield factorGenerator(Seq(above,(above._1,above._2 + 1)).map(variables),domains,random)
    val singletons = variables.keys.map(v => factorGenerator(Seq(variables(v)),domains,random))
    Problem((singletons ++ horizontalPairs ++ verticalPairs)(collection.breakOut), domains, NormalD)

  }
  def randomK(numVariables: Int,
              numFactors: Int,
              factorSize: Int,
              domainSize: Int,
              factorGenerator: FactorGenerator,
              random: Random): Problem = {
    val domains = Array.fill(numVariables)(domainSize)
    def genFactorVariables: Array[Int] = Iterator
      .continually(Array.fill(factorSize)(random.nextInt(numVariables)))
      .filter(_.toSet.size == factorSize)
      .next()
    Problem(Array.fill(numFactors)(factorGenerator(genFactorVariables,domains,random)),domains,NormalD)
  }

}
