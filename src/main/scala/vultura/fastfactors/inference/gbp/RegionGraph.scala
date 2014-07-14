package vultura.fastfactors.inference.gbp

import vultura.fastfactors.{FastFactor, Problem}
import vultura.util.SSet
import vultura.util.graph.EdgeMapDiGraph

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

trait RegionGraph {
  def problem: Problem

  def regions: Set[Region]
  def edges: Set[(Region,Region)]
  def hyperEdges: Map[Region,Set[Region]] =
    regions.map(_ -> Set[Region]()).toMap ++
      edges.groupBy(_._2).map{case (k,vs) => k -> vs.map(_._2).toSet}

  def calculateCountingNumbers: RegionGraph

  def variableCount(v: Int): Double
  def factorCount(f: FastFactor): Double
  /** @return true if subgraph induced by containing given factor is connected. */
  def factorConnectedness(f: FastFactor): Boolean
  /** @return true if subgraph induced by containing given variable is connected. */
  def variableConnectedness(variable: Int): Boolean

  def checks: Seq[Option[String]] = Seq(
    if(problem.hasDuplicateFactors) Some("the problem contains duplicate factors") else None,
    problem.variables.find(v => math.abs(variableCount(v) - 1) > 1e-10).map(v => "variable counting numbers don't add to one for variable " + v),
    problem.factors.find(f => math.abs(factorCount(f) - 1) > 1e-10).map("factor counting numbers don't add to one for factor " + _),
    problem.factors.find(!factorConnectedness(_)).map("induced subgraph is not connected for each factor " + _),
    problem.variables
      .map(v => EdgeMapDiGraph(regions,edges).subgraph(_.variables.contains(v)))
      .find(_.toBiMap.components.size > 1)
      .map("induced subgraph is not connected for some variable: " + _),
    edges.find{ case (Region(_,hiVs,hiFs), Region(_,loVs,loFs)) =>
      !(loVs.subsetOf(hiVs) && loFs.subsetOf(hiFs)) }.map("child region is no subset of parent region for edge " + _),
    regions.find(!_.hasValidScope).map("there exists a region not encompassing its factors scopes: " + _),
    EdgeMapDiGraph(regions,edges).findCycle.map("graph has a cycle: " + _)
  )

  def issues: Seq[String] = checks.flatten

  def toDot: String = {
    val nodeId: Map[Region, String] = regions.zipWithIndex.toMap.mapValues("n" + _)
    val factorId = problem.factors.zipWithIndex.toMap
    def nodeLabel(n: Region): String = f"[${n.cr}] ${(n.variables ++ n.factors.map(f => "f" + factorId(f))).mkString(",")}"
    val nodeString = regions.map(n => f"""${nodeId(n)} [label = "${nodeLabel(n)}"];""").mkString("\n")
    val edgeString = edges.map{case (from,to) => f"${nodeId(from)} -> ${nodeId(to)};"}.mkString("\n")

   f"""digraph RegionGraph {
      | rankdir = TB;
      | $nodeString
      | $edgeString
      |}
    """.stripMargin
  }
}

case class MapRegionGraph(problem: Problem, regions: Set[Region], edges: Set[(Region,Region)]) extends RegionGraph {
  val forwardMap: Map[Region, Set[Region]] = EdgeMapDiGraph.calculateForwardMap(edges).withDefaultValue(Set[Region]())
  val backwardMap: Map[Region,Set[Region]] = EdgeMapDiGraph.calculateBackwardMap(edges).withDefaultValue(Set[Region]())

  def variableCount(v: Int): Double = regions.filter(_.variables.contains(v)).toSeq.map(_.cr).sum
  def factorCount(f: FastFactor): Double = regions.filter(_.factors.contains(f)).toSeq.map(_.cr).sum
  /** @return true if subgraph induced by containing given factor is connected. */
  def factorConnectedness(f: FastFactor): Boolean =
    EdgeMapDiGraph(regions,forwardMap).subgraph(_.factors.contains(f)).toBiMap.components.size == 1
  /** @return true if subgraph induced by containing given variable is connected. */
  def variableConnectedness(variable: Int): Boolean =
    EdgeMapDiGraph(regions,forwardMap).subgraph(_.variables.contains(variable)).toBiMap.components.size == 1

  def calculateCountingNumbers: RegionGraph = {
    val backClosure = vultura.util.transitiveClosure(backwardMap)
    def calcCR(mapping: Map[Region,Double]): Map[Region,Double] = {
      val newMaps = for{
        x <- mapping.keys
        next <- forwardMap(x) if !mapping.contains(next) && backwardMap(next).forall(mapping.contains)
      } yield next -> (1 - backClosure(next).toSeq.map(mapping).sum)
      if(newMaps.isEmpty) mapping else calcCR(mapping ++ newMaps)
    }
    val crs = calcCR(regions.filterNot(r => forwardMap(r).isEmpty).map(_ -> 1d)(collection.breakOut))
    def mapRegion(r: Region): Region = r.copy(cr=crs(r))
    MapRegionGraph(problem,regions.map(mapRegion),edges.map{case (a,b) => (mapRegion(a),mapRegion(b))})
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

    val edges: Iterable[(Region,Region)] = for{
      (f,fr) <- factorRegions
      v <- f.variables
      vr = variableRegions(v)
    } yield fr -> vr

    MapRegionGraph(problem,regions.toSet,edges.toSet)
  }

  /** See (Koller et Friedman, 2009,p 423). */
  def saturatedRG(problem: Problem, _initialRegions: Iterable[(Set[Int],Iterable[FastFactor])]): RegionGraph = {
    require(_initialRegions.map(_._1).flatten.toSet.subsetOf(problem.variables.toSet))
    require(_initialRegions.map(_._2).flatten.toSet == problem.factors.toSet)

    def nonemptyIntersections(rs: Set[Set[Int]]): Set[Set[Int]] =
      for(s1 <- rs; s2 <- rs; intersect = s1 intersect s2 if !intersect.isEmpty) yield intersect

    val initialRegions = _initialRegions.map(_._1).toSet
    val regionToFactor = _initialRegions.toMap.mapValues(_.toSet)

    val allSets = setClosure(initialRegions)(nonemptyIntersections)
    val allRegions = allSets.map(vars => Region(1d,vars,regionToFactor.getOrElse(vars,Set())))

    val edges: Set[(Region,Region)] = for{
      r1 <- allRegions
      r2 <- allRegions if (r1 != r2) && r2.variables.subsetOf(r1.variables) && !allRegions.exists(r3 => r1 != r3 && r2 != r3 &&  r2.variables.subsetOf(r3.variables) && r3.variables.subsetOf(r1.variables))
    } yield r1 -> r2

    MapRegionGraph(problem, allRegions, edges).calculateCountingNumbers
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
  }.dropWhile(_._2).next()._1
}
