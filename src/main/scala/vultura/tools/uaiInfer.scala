package vultura.tools

import org.rogach.scallop.{ValueConverter, ScallopConf}
import java.io._
import org.rogach.scallop
import scala.Some
import vultura.factors.{ProductFactor, uai, TableFactor}
import vultura.fastfactors.{FastFactor, RingZ, LogD}
import scala.collection.immutable.IndexedSeq
import vultura.util.{RingWithZero, Benchmark}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 5/31/13
 */
object uaiInfer {
  class Config(args: Seq[String]) extends ScallopConf(args) {
    implicit val fileConverter: ValueConverter[File] = scallop.singleArgConverter(new File(_))
    implicit val inStream = scallop.singleArgConverter[InputStream](new FileInputStream(_))
    implicit val outStream = scallop.singleArgConverter[OutputStream](new FileOutputStream(_))

    banner("Perform actions on markov networks in `uai` format to structurally equivalent SAT instances in `cnf` format.")
    footer("this is the footer")
    version("uai2uai 1.0")

    val input = opt[InputStream](
      name="input",
      short='i',
      descr="file to read from (uai-format); stdin if not given",
      default=Some(System.in)
    )
    val task = trailArg[String](
      name = "task",
      descr = "select for task [MAR|PR]",
      validate = str => Seq("mar","pr").contains(str.toLowerCase),
      default = Some("PR")
    )
    val condition = opt[String](
      name = "condition-on",
      descr = "condition on variables, given as comma or space separated list before starting inference",
      default = None
    )
  }

  def main(args: Array[String]) {
    val config = new Config(args)

    val inProblem: Seq[TableFactor[Double]] = uai.parseUAIMarkov(config.input())

    val domains: Array[Int] = {
      val varDomainMap: Seq[(Int, Int)] =
        inProblem.flatMap(f => f.variables.zip(f.domains.map(_.size)))
      assert(varDomainMap.toMap.size == varDomainMap.distinct.size, "differing variable domains")
      val maxvar = varDomainMap.map(_._1).max
      (0 to maxvar map varDomainMap.toMap.withDefaultValue(0))(collection.breakOut)
    }

    //map to logdomain
    val (problem: IndexedSeq[FastFactor],ring: RingZ[Double]) = (inProblem.map(_.map(math.log)).map(f => FastFactor.orderIfNecessary(f.variables,f.denseData,domains)).toIndexedSeq,LogD)

    val resultFactor = variableElimination(problem, ring, domains)

    println(
      Seq.fill(100)(Benchmark.benchmarkCPUTime(variableElimination(problem, ring, domains)))
        .map{case(r,time) => "t: " + time.toDouble * 1e-9 + " v: " + r.head.values(0)}
        .last
    )
    println("old code")
    val pf = ProductFactor(inProblem,RingWithZero.logSumProd.multiplication)
    println(
      Seq.fill(100)(Benchmark.benchmarkCPUTime(pf.jtPartition(RingWithZero.logSumProd.addition)))
        .map{case(r,time) => "t: " + time.toDouble * 1e-9 + " v: " + r}
        .last
      )

    println(resultFactor.head.values(0))
  }

  def variableElimination(problem: IndexedSeq[FastFactor], ring: RingZ[Double], domains: Array[Int]): List[FastFactor] = {
    val ordering: List[Int] = vultura.util.TreeWidth.minDegreeOrdering(problem.map(_.variables.toSet))

    ordering.foldLeft(problem.toList) {
      case (factors, elimVar) =>
        val (eliminatedFactors, remainingFactors) = factors.partition(_.variables.contains(elimVar))
        val product = FastFactor.multiplyMarginalize(ring)(domains)(eliminatedFactors, Array(elimVar))
        product :: remainingFactors
    }
  }
}
