package vultura.wsat

import java.io.File
import vultura.factors.{ProductFactor, TableFactor}
import vultura.util.RingWithZero

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 3/15/13
 */
object Test {
  def main(args: Array[String]) {
    val uaiProblem: Seq[TableFactor[Double]] = vultura.factors.uai.parseUAIMarkovFromFile(new File(args(0)))
    println(math.log(ProductFactor(uaiProblem,RingWithZero.sumProduct.multiplication).jtPartition(RingWithZero.sumProduct.addition)))
    val problem = productToClauseWeightedKB(uaiProblem)
    println(problem)
    println(math.log(problem.sumProductBrute))
  }
}
