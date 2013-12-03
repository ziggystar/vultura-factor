package vultura.tools

import org.rogach.scallop.{ValueConverter, ScallopConf}
import java.io._
import org.rogach.scallop
import vultura.fastfactors._
import vultura.util.Benchmark
import scala.util.Random
import vultura.fastfactors.Problem
import vultura.util.IntDomainCPI
import vultura.fastfactors.algorithms.{BeliefPropagation, CBP}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 5/31/13
 */
object uaiInfer {
  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

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
      descr = "select for task [PR|MAR]",
      validate = str => Seq("mar","pr").contains(str.toLowerCase),
      default = Some("PR")
    )
    val condition = opt[String](
      name = "condition-on",
      descr = "condition on variables, given as comma or space separated list before starting inference",
      default = None
    )
    val algorithm = opt[String](
      name = "algorithm",
      short = 'a',
      default = Some("BP")
    )
    val useLog = opt[Boolean](
      name = "uselog",
      descr = "perform computations in log-domain",
      default = Some(false)
    )
    val benchmark= opt[Boolean](
      name = "benchmark",
      default = Some(false)
    )
  }

  def main(args: Array[String]) {
    val config = new Config(args)

    val inProblem: Problem = Problem.parseUAIProblem(config.input()).right.get

    val ring = if(config.useLog()) LogD else NormalD

    logger.info(f"Loaded problem with ${inProblem.factors.size} factors and ${inProblem.variables.size} variables")

    val problem = simplifyProblem(inProblem)
    logger.info(f"simplified ${inProblem.factors.size} factors to ${problem.factors.size}")

    val conditioningVariables: Seq[Int] = config.condition.get.map(_.split(",").toSeq.map(_.toInt)).getOrElse(Seq[Int]())
    val cpi = new IntDomainCPI(conditioningVariables.map(problem.domains).map(x => (0 until x).toArray).toArray)
    val random = new Random

    logger.info(f"running algorithm ${config.algorithm()}")
    val infer: Problem => Double = config.algorithm().toUpperCase match {
      case "CBP" => { problem =>
        val cbp = new CBP(problem,CBP.LEAF_SELECTION.RANDOM,CBP.VARIABLE_SELECTION(CBP.VARIABLE_SELECTION.RANDOM),CBP.CLAMP_METHOD.CLAMP,50,1e-10,random)
        cbp.run(30)
        cbp.logZ
      }

      case "BP" => { problem =>
        val bp = new BeliefPropagation(problem,random,1e-7,1000)
        if(!bp.converged)
          logger.warning(f"BP did not converge after ${bp.iterations} iterations with ${bp.getMessageUpdates} message updates; remaining message delta of ${bp.maxDelta}")
        else
          logger.fine(f"BP converged after ${bp.iterations} iterations with ${bp.getMessageUpdates} message updates; remaining message delta of ${bp.maxDelta}")
        if(config.useLog()) bp.logZ else math.exp(bp.logZ)
      }
      case "JTREE" => veJunctionTree
      case "VE" => variableElimination
    }

    def calcCondZs: IndexedSeq[Double] = cpi.map{ assignment =>
      val cond = conditioningVariables.zip(assignment).toMap
      val (conditionedProblem,conditionedDomain) = conditionProblem(problem.factors,problem.domains,cond)
      logger.fine(f"conditioning on assignment: ${assignment.mkString(":")}")
      infer(Problem(conditionedProblem,conditionedDomain,ring))
    }

    if(config.benchmark()){
      logger.info("running warmup for at least 5s")
      //warmup
      val wut = System.nanoTime
      while(System.nanoTime - wut < 5e9){
        calcCondZs
      }
      logger.info("running benchmark for at least 20s")
      //measure
      val startTime = System.nanoTime
      var i = 0
      val (_,time) = Benchmark.benchmarkCPUTime{
        while(System.nanoTime - startTime < 20e9){
          calcCondZs
          i += 1
        }
      }
      println("CPU time average over " + i + " runs: " + time.toDouble*1e-9/i)
    }

    val conditionedZs = calcCondZs
    val Z: Double = ring.sumA(conditionedZs.toArray)
    println("ln(Z) = " + (if(config.useLog()) Z else math.log(Z)))
  }

  def conditionProblem(problem: Seq[FastFactor], domains: Array[Int], condition: Map[Int,Int]): (IndexedSeq[FastFactor], Array[Int]) = {
    val condProblem: IndexedSeq[FastFactor] = problem.map(_.condition(condition,domains))(collection.breakOut)
    val newDomains = domains.zipWithIndex.map{case (d,v) => if(condition.contains(v)) 1 else d}
    val simplifiedProblem = condProblem.map(_.simplify(newDomains))
    (simplifiedProblem,newDomains)
  }

  def partialFoldGraph[A](graph: Map[A,Set[A]])(contract: PartialFunction[(A,A),A]): Map[A,Set[A]] = {
    def foldR(g: Map[A,Set[A]])(unvisited: List[(A,A)]): Map[A,Set[A]] = unvisited match {
      case Nil => g
      case (e@(a,b)) :: rest if a != b && g.contains(a) && g.contains(b) && contract.isDefinedAt(e) => {
        val contractionResult@(newNode, affectedNeighbours) = contract(e) -> (g(a) ++ g(b)).filterNot(v => v == a || v == b)
        val newGraph = g --
          Seq(a,b) + //remove nodes a,b
          contractionResult ++ //add newNode
          affectedNeighbours.map(n => n -> (g(n) -- Seq(a,b) + newNode)) //update all affectedNeighbours to point to newNode instead of a or b
        val newEdgeList = affectedNeighbours.map(newNode -> _) ++: rest
        foldR(newGraph)(newEdgeList)
      }
      case _ :: rest => foldR(g)(rest)
    }
    foldR(graph)(graph.flatMap{case (src, sinks) => sinks.map(src -> _)}(collection.breakOut))
  }

  /** Multiplies factors into super-set factors. */
  def simplifyProblem(p: Problem): Problem = {
    val factorsOfvar: Map[Int,Set[FastFactor]] = dualBipartiteGraph(p.factors.map(f => f -> f.variables.toSet)(collection.breakOut))
    val primalGraph: Map[FastFactor,Set[FastFactor]] = p.factors.map(f => f -> f.variables.flatMap(factorsOfvar).toSet)(collection.breakOut)
    val factorContraction: PartialFunction[(FastFactor,FastFactor),FastFactor] = {
      case (fa,fb) if fa.variables.toSet.subsetOf(fb.variables.toSet) || fb.variables.toSet.subsetOf(fa.variables.toSet) =>
        FastFactor.multiply(p.ring)(p.domains)(IndexedSeq(fa,fb))
    }
    val contractedGraph: Map[FastFactor, Set[FastFactor]] = partialFoldGraph(primalGraph)(factorContraction)
    p.copy(factors=contractedGraph.keysIterator.toIndexedSeq)
  }

  def dualBipartiteGraph[A,B](g: Map[A,Iterable[B]]): Map[B,Set[A]] =
    (g.flatMap{case (a, bs) => bs.map(b => b -> a)}(collection.breakOut): IndexedSeq[(B,A)])
      .groupBy(_._1).map{case (b, bas) => b -> bas.map(_._2).toSet}
}