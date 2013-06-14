package vultura.tools

import org.rogach.scallop.{ValueConverter, ScallopConf}
import java.io._
import org.rogach.scallop
import vultura.factors.{uai, TableFactor}
import vultura.fastfactors._
import vultura.util.{Benchmark, TreeWidth, IntDomainCPI}
import scala.util.Random
import vultura.fastfactors.algorithms.{CBP, BeliefPropagation}

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

    val inProblem: Seq[TableFactor[Double]] = uai.parseUAIMarkov(config.input())

    val domains: Array[Int] = {
      val varDomainMap: Seq[(Int, Int)] =
        inProblem.flatMap(f => f.variables.zip(f.domains.map(_.size)))
      assert(varDomainMap.toMap.size == varDomainMap.distinct.size, "differing variable domains")
      val maxvar = varDomainMap.map(_._1).max
      (0 to maxvar map varDomainMap.toMap.withDefaultValue(0))(collection.breakOut)
    }

    val ring = if(config.useLog()) LogD else NormalD

    logger.info(f"Loaded problem with ${inProblem.size} factors and ${inProblem.flatMap(_.variables).toSet.size} variables")

    val problem: IndexedSeq[FastFactor] = {
      val preProblem = inProblem
        .toIndexedSeq
        .map(f => FastFactor.orderIfNecessary(f.variables,f.denseData,domains))
        .map{ case FastFactor(vars, values) => FastFactor(vars, ring.encode(values))}
      val simplified = simplifyFactorSet(preProblem,ring,domains)
      logger.info(f"simplified ${preProblem.size} factors to ${simplified.size}")
      simplified
    }

    val conditioningVariables: Seq[Int] = config.condition.get.map(_.split(",").toSeq.map(_.toInt)).getOrElse(Seq[Int]())
    val cpi = new IntDomainCPI(conditioningVariables.map(domains).map(x => (0 until x).toArray).toArray)
    val random = new Random

    logger.info(f"running algorithm ${config.algorithm()}")
    val infer: Problem => Double = config.algorithm().toUpperCase match {
      case "CBP" => { problem =>
        val cbp = new CBP(problem,random,CBP.leafSelectionSlowestSettler,CBP.variableSelectionSlowestSettler,50,1e-5)
        cbp.run(30)
        cbp.logZ
      }

      case "BP" => { problem =>
        val bp = new BeliefPropagation(problem)
        val maxiter: Int = 1000
        bp.run(maxiter,1e-7)
        if(!bp.converged)
          logger.warning(f"BP did not converge after $maxiter iterations with ${bp.getMessageUpdates} message updates; remaining message delta of ${bp.maxDelta}")
        else
          logger.fine(f"BP converged after ${bp.iterations} iterations with ${bp.getMessageUpdates} message updates; remaining message delta of ${bp.maxDelta}")
        if(config.useLog()) bp.logZ else math.exp(bp.logZ)
      }
      case "JTREE" => p => veJunctionTree(p.factors,p.ring,p.domains)
      case "VE" => p => variableElimination(p.factors,p.ring,p.domains)
    }

    def calcCondZs: IndexedSeq[Double] = cpi.map{ assignment =>
      val cond = conditioningVariables.zip(assignment).toMap
      val (conditionedProblem,conditionedDomain) = conditionProblem(problem,domains,cond)
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

  def simplifyFactorSet(problem: IndexedSeq[FastFactor], ring: RingZ[Double], domains: Array[Int]): IndexedSeq[FastFactor] = {
    val factorsOfvar: Map[Int,Set[FastFactor]] = dualBipartiteGraph(problem.map(f => f -> f.variables.toSet)(collection.breakOut))
    val primalGraph: Map[FastFactor,Set[FastFactor]] = problem.map(f => f -> f.variables.flatMap(factorsOfvar).toSet)(collection.breakOut)
    val factorContraction: PartialFunction[(FastFactor,FastFactor),FastFactor] = {
      case (fa,fb) if fa.variables.toSet.subsetOf(fb.variables.toSet) || fb.variables.toSet.subsetOf(fa.variables.toSet) =>
        FastFactor.multiply(ring)(domains)(IndexedSeq(fa,fb))
    }
    val contractedGraph: Map[FastFactor, Set[FastFactor]] = partialFoldGraph(primalGraph)(factorContraction)
    contractedGraph.keysIterator.toIndexedSeq
  }

  def dualBipartiteGraph[A,B](g: Map[A,Iterable[B]]): Map[B,Set[A]] =
    (g.flatMap{case (a, bs) => bs.map(b => b -> a)}(collection.breakOut): IndexedSeq[(B,A)])
      .groupBy(_._1).map{case (b, bas) => b -> bas.map(_._2).toSet}
}