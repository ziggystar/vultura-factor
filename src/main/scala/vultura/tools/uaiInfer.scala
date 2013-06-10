package vultura.tools

import org.rogach.scallop.{ValueConverter, ScallopConf}
import java.io._
import org.rogach.scallop
import scala.Some
import vultura.factors.{uai, TableFactor}
import vultura.fastfactors.{FastFactor, RingZ, LogD}
import scala.collection.immutable.IndexedSeq
import vultura.util.{IntDomainCPI, TreeWidth}

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

    banner("Perform actions on markov networks in `uai` format.")
    version("uai2uai 1.0")

    val input = opt[InputStream](
      name="input",
      short='i',
      descr="file to read from (uai-format); stdin if not given",
      default=Some(System.in)
    )
    val task = trailArg[String](
      name = "task",
      descr = "select for task [PR]",
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

    val conditioningVariables: Seq[Int] = config.condition.get.map(_.split(",").toSeq.map(_.toInt)).getOrElse(Seq[Int]())
    val cpi = new IntDomainCPI(conditioningVariables.map(domains).map(x => (0 until x).toArray).toArray)

    def calcCondZs = cpi.map{ assignment =>
      val cond = conditioningVariables.zip(assignment).toMap
      val (conditionedProblem,conditionedDomain) = conditionProblem(problem,domains,cond)
//      veJunctionTree(conditionedProblem,ring,conditionedDomain)
      variableElimination(conditionedProblem,ring,conditionedDomain)
    }
    val benchmark = true
    if(benchmark){
      //warmup
      val wut = System.nanoTime
      while(System.nanoTime - wut < 5e9){
        calcCondZs
      }
      //measure
      val startTime = System.nanoTime
      var i = 0
      while(System.nanoTime - startTime < 20e9){
        calcCondZs
        i += 1
      }
      val time = System.nanoTime - startTime
      println("average over " + i + " runs: " + time.toDouble*1e-9/i)
    }

    val conditionedZs = calcCondZs

    println("ln(Z) = " + ring.sumA(conditionedZs.toArray))

  }

  def conditionProblem(problem: Seq[FastFactor], domains: Array[Int], condition: Map[Int,Int]): (IndexedSeq[FastFactor], Array[Int]) = {
    val condProblem: IndexedSeq[FastFactor] = problem.map(_.condition(condition,domains))(collection.breakOut)
    val newDomains = domains.zipWithIndex.map{case (d,v) => if(condition.contains(v)) 1 else d}
    val simplifiedProblem = condProblem.map(_.simplify(newDomains))
    (simplifiedProblem,newDomains)
  }

  def veJunctionTree(problem: IndexedSeq[FastFactor], ring: RingZ[Double], domains: Array[Int]): Double = {
    import TreeWidth._
    import scalaz._
    import Scalaz._
    val trees: Seq[Tree[(Set[Int], Seq[FastFactor])]] = compactJTrees(minDegreeJTs(problem.map(f => f.variables.toSet -> f)))
    def veR(tree: Tree[(Set[Int], Seq[FastFactor])]): (Set[Int], Seq[FastFactor]) = tree match {
      case Node((vars,factors), sf) if !sf.isEmpty =>
        (vars,factors ++ sf.map(veR(_)).map(fs => FastFactor.multiplyMarginalize(ring)(domains)(fs._2,(fs._1 -- vars).toArray)))
      case Node(root, _) => root
    }
    ring.prodA(
      trees
        .map(veR)
        .map{case (vars,factors) => FastFactor.multiplyMarginalize(ring)(domains)(factors,vars.toArray).values(0)}(collection.breakOut)
    )
  }

  def variableElimination(problem: IndexedSeq[FastFactor], ring: RingZ[Double], domains: Array[Int]): Double = {
    val graph: IndexedSeq[Set[Int]] = problem.map(_.variables.toSet)
//    val (ordering: List[Int], potentialSize) = TreeWidth.vertexOrdering(TreeWidth.weightedMinDegree(domains))(graph)
    val (ordering: List[Int], _) = TreeWidth.minDegreeOrderingAndWidth(graph)

    val eliminationResult: List[FastFactor] = ordering.foldLeft(problem.toList) {
      case (factors, elimVar) =>
        val (eliminatedFactors, remainingFactors) = factors.partition(_.variables.exists(_ == elimVar))
        val product = FastFactor.multiplyMarginalize(ring)(domains)(eliminatedFactors, Array(elimVar))
        product :: remainingFactors
    }
    val constants: Array[Double] = eliminationResult.map(_.values.head)(collection.breakOut)
    ring.prodA(constants)
  }
}