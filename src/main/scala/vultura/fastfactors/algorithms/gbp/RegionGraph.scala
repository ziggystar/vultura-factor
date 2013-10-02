package vultura.fastfactors.algorithms.gbp

import vultura.fastfactors.{Problem, FastFactor}
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import vultura.util.SSet
import scala.util.Random

case class Region(cr: Double, variables: Set[Int], factors: Set[FastFactor]){
  /** @return true if the union of factor scopes is a subset of the variables. */
  def hasValidScope: Boolean = factors.flatMap(_.variables).subsetOf(variables)
  override val hashCode: Int = (variables,factors).hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case Region(_,v,f) => (variables,factors) == (v,f)
    case _ => false
  }
}

object Region{
  def fromFactors(fs: Iterable[FastFactor], cr: Double = 1): Region = {
    val fSet = fs.toSet
    require(fSet.size == fs.size)
    Region(cr,fSet.flatMap(_.variables).toSet,fSet)
  }
}

/**
 * Encodes a region graph.
 */
case class RegionGraph protected(problem: Problem, graph: Graph[Region, DiEdge]) {
  def checks: Seq[(Boolean,String)] = Seq(
    (!problem.hasDuplicateFactors, "the problem contains duplicate factors"),
    (problem.variables.forall(v => math.abs(variableCount(v) - 1) < 1e-10), "variable counting numbers don't add to one"),
    (problem.factors.forall(f => math.abs(factorCount(f) - 1) < 1e-10), "factor counting numbers don't add to one"),
    (problem.factors.forall(factorConnectedness), "induced subgraph is not connected for each factor"),
    (problem.variables.forall(variableConnectedness), "induced subgraph is not connected for each variable"),
    (graph.edges.map(_.toEdgeIn).forall{ edge =>
      val (Region(_,hiVs,hiFs), Region(_,loVs,loFs)) = (edge.from,edge.to)
      loVs.subsetOf(hiVs) && loFs.subsetOf(hiFs) },
      "child region is no subset of parent region for some edge"),
    (graph.nodes.forall(_.hasValidScope), "there exists a region not encompassing its factors scopes"),
    (graph.isAcyclic, "graph has a cycle")
  )

  def issues: Seq[String] = checks.foldLeft(Nil: List[String]){
    case (acc,(false,_)) => acc
    case (acc,(true,msg)) => acc :+ msg
  }

  def isValid: Boolean = issues.isEmpty

  def factorCount(f: FastFactor): Double = (for( Region(cr,_,fs) <- graph.nodes.toOuterNodes if fs.contains(f) ) yield cr).sum
  def factorConnectedness(f: FastFactor): Boolean = (graph --! graph.nodes.filterNot(_.factors.contains(f))).isConnected
  def variableCount(variable: Int): Double = (for( Region(cr,vs,_) <- graph.nodes.toOuterNodes if vs.contains(variable) ) yield cr).sum
  def variableConnectedness(variable: Int): Boolean = (graph --! graph.nodes.filterNot(_.variables.contains(variable))).isConnected

  /** @return This RegionGraph with the counting numbers calculated correctly. */
  def calculateCountingNumbers: RegionGraph = {
    println("calculating counting numbers is not implemented !!!")
    this
  }

  def toDot: String = {
    val nodeId: Map[graph.NodeT, String] = graph.nodes.zipWithIndex.toMap.mapValues("n" + _)
    val factorId = problem.factors.zipWithIndex.toMap
    def nodeLabel(n: graph.NodeT): String = (n.variables ++ n.factors.map(f => "f" + factorId(f))).mkString(",")
    val nodeString = graph.nodes.map(n => f"""${nodeId(n)} [label = "${nodeLabel(n)}"];""").mkString("\n")
    val edgeString = graph.edges.map(e => f"${nodeId(e.from)} -> ${nodeId(e.to)};").mkString("\n")

   f"""digraph RegionGraph {
      | rankdir = TB;
      | ${nodeString}
      | ${edgeString}
      |}
    """.stripMargin
  }
}

object RegionGraph{
  /** Construct a the Bethe factor graph as a region graph. */
  def betheRG(problem: Problem): RegionGraph = {
    val variableRegions: Map[Int, Region] = (for{
      v <- problem.variables
      cr = 1 - problem.degreeOfVariable(v)
    } yield v -> Region(cr, Set(v), Set())).toMap

    val factorRegions: Map[FastFactor, Region] = (for{
      f <- problem.factors
    } yield f -> Region.fromFactors(Set(f),1)).toMap

    val regions = variableRegions.values ++ factorRegions.values

    val edges: Iterable[DiEdge[Region]] = for{
      (f,fr) <- factorRegions
      v <- f.variables
      vr = variableRegions(v)
    } yield fr ~> vr

    val graph = Graph.from(regions,edges)

    RegionGraph(problem,graph)
  }

  /** See (Koller et Friedman, 2009,p 423). */
  def saturatedRG(problem: Problem, _initialRegions: Iterable[(Set[Int],Iterable[FastFactor])]): RegionGraph = {
    require(_initialRegions.map(_._1).flatten.toSet.subsetOf(problem.variables.toSet))
    require(_initialRegions.map(_._2).flatten.toSet == problem.factors.toSet)

    def allIntersections(rs: Set[Set[Int]]): Set[Set[Int]] = for(s1 <- rs; s2 <- rs) yield s1 intersect s2
    def validNewRegions(rs: Set[Set[Int]]): Set[Set[Int]] = allIntersections(rs)
      .filterNot(_.isEmpty) //remove empty intersections
      .filterNot(succ => rs.exists(pred => succ.subsetOf(pred))) //remove subsets of old regions

    val initialRegions = _initialRegions.map(_._1).toSet
    val regionToFactor = _initialRegions.toMap.mapValues(_.toSet)

    val allSets = setClosure(initialRegions)(validNewRegions)
    val allRegions = allSets.map(vars => Region(1d,vars,regionToFactor.getOrElse(vars,Set())))

    val edges: Set[DiEdge[Region]] = for{
      r1 <- allRegions
      r2 <- allRegions if (r1 != r2) && r2.variables.subsetOf(r1.variables) && !allRegions.exists(r3 => r2.variables.subsetOf(r3.variables) && r3.variables.subsetOf(r1.variables))
    } yield r1 ~> r2

    RegionGraph(problem, Graph.from(allRegions, edges)).calculateCountingNumbers
  }

  def saturatedRG(problem: Problem, random: Random): RegionGraph = {
    val factorCliques = problem.factors.map(_.variables.toSet).toSet
    val regions = new SSet(factorCliques).maximalSets
    val regionsSSet = new SSet(regions)
    import vultura.util._
    val factorRegions: Map[Set[Int], IndexedSeq[FastFactor]] = problem.factors.groupBy(f => regionsSSet.superSetsOf(f.variables.toSet).pickRandom(random))
    saturatedRG(problem,factorRegions)
  }

  def setClosure[A](init: Set[A])(f: Set[A] => Set[A]): Set[A] = Iterator.iterate((init,true)){
    case x@(acc,false) => x
    case (acc,true) => {
      val add = f(acc).filterNot(acc)
      (acc ++ add,!add.isEmpty)
    }
  }.dropWhile(!_._2).next._1
}
