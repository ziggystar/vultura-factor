package vultura.fastfactors.algorithms.gbp

import vultura.fastfactors.{Problem, FastFactor}
import scalax.collection._
import scalax.collection.Graph // or scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

case class RGNode(id: Int, cr: Double, variables: Set[Int], factors: Set[FastFactor]){
  override def hashCode(): Int = id

  override def equals(obj: scala.Any): Boolean = obj match {
    case RGNode(otherId,_,_,_) => id == otherId
    case _ => false
  }
}

/**
 * Encodes a region graph.
 */
case class RegionGraph protected(problem: Problem, graph: Graph[RGNode, DiEdge]) {
  //we don't want problems with duplicate factors
  require(!problem.hasDuplicateFactors, "there are duplicate factors")

  //the following checks ensure the region graph properties.
  require(problem.variables.forall(v => math.abs(variableCount(v) - 1) < 1e-10), "variable counting numbers don't add to one")
  require(problem.factors.forall(f => math.abs(factorCount(f) - 1) < 1e-10), "factor counting numbers don't add to one")
  require(problem.factors.forall(factorConnectedness), "induced subgraph is not connected for each factor")
  require(problem.variables.forall(variableConnectedness), "induced subgraph is not connected for each variable")
  require(
    graph.edges.map(_.toEdgeIn).forall{ edge =>
      val (RGNode(_,_,hiVs,hiFs), RGNode(_,_,loVs,loFs)) = (edge.from,edge.to)
      loVs.subsetOf(hiVs) && loFs.subsetOf(hiFs) },
    "child region is no subset of parent region for some edge"
  )

  require(
    graph.nodes.toOuterNodes.forall{case RGNode(_,_,vs,fs) => fs.forall(_.variables.toSet.subsetOf(vs))},
    "there exists a region not encompassing its factors scopes")

  def factorCount(f: FastFactor): Double = (for( RGNode(_,cr,_,fs) <- graph.nodes.toOuterNodes if fs.contains(f) ) yield cr).sum
  def factorConnectedness(f: FastFactor): Boolean = (graph --! graph.nodes.filterNot(_.factors.contains(f))).isConnected
  def variableCount(variable: Int): Double = (for( RGNode(_,cr,vs,_) <- graph.nodes.toOuterNodes if vs.contains(variable) ) yield cr).sum
  def variableConnectedness(variable: Int): Boolean = (graph --! graph.nodes.filterNot(_.variables.contains(variable))).isConnected
}

object RegionGraph{
  def betheRG(problem: Problem): RegionGraph = ???
  def saturatedRG(problem: Problem): RegionGraph = ???
}
