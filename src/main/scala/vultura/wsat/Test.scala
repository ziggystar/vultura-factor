package vultura.wsat

import java.io.File

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 3/15/13
 */
object Test {
  def main(args: Array[String]) {
    val problem = productToClauseWeightedKB(vultura.factors.uai.parseUAIMarkovFromFile(new File(args(0))))
    println(problem)
    println(problem.sumProductBrute)
  }
}
