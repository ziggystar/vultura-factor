package vultura.factor.generation

import vultura.factor._
import vultura.util.{IntDomainCPI, Index, SIIndex}

import scala.util.Random

/** A Hypergraph without parallel edges. */
case class Graph[N](nodes: Set[N], edges: Set[Set[N]]){
  def filterNodes(p: N => Boolean): Graph[N] = Graph(nodes filter p,edges filter (_.forall(p)))
  def filterEdges(p: Set[N] => Boolean): Graph[N] = Graph(nodes, edges filter p)
  /** Adds a new edge to the graph, if it covers a new vertex, it will be silently added. */
  def addEdge(e: Set[N]): Graph[N] = Graph(nodes ++ e, edges + e)
  def map[M](f: N => M): Graph[M] = Graph(nodes map f, edges.map(_.map(f)))
  def neighboursOf(n: N): Set[N] = incidentEdgesOf(n).flatten - n
  def incidentEdgesOf(n: N): Set[Set[N]] = edges.filter(_.contains(n))
}

case class LabeledProblemStructure[N](structure: ProblemStructure, variableLabels: Index[N]){
  require(variableLabels.size == structure.numVariables)
}

object GraphGenerator {
  def erdoesRenyi(nodes: Int, p: Double): Generator[Graph[Int]] = ???
  /** Build a n-dimensional lattice.
    * First in tuple is dimension size, second is toroidal condition on this dimension (true means wrapping). */
  def lattice(dimensions: (Int,Boolean)*): Graph[IndexedSeq[Int]] = {
    val variables = new SIIndex[IndexedSeq[Int]](IntDomainCPI(dimensions.map(_._1).map(0 until).map(_.toArray).toArray).seq.map(_.toIndexedSeq))
    def nodeValid(coords: IndexedSeq[Int]): Option[IndexedSeq[Int]] = {
      val fixed = coords.zip(dimensions).map {
        case (xi, (di, _)) if xi >= 0 && xi < di => Some(xi)
        case (xi, (di, true)) if xi == -1 => Some(di - 1)
        case (xi, (di, true)) if xi == di => Some(0)
        case _ => None
      }
      val filtered = fixed.flatten
      if(filtered.length == fixed.length) Some(filtered)
      else None
    }
    val edges: IndexedSeq[(IndexedSeq[Var], IndexedSeq[Var])] = for {
      node <- variables.elements
      shift <- dimensions.indices.map(i => node.updated(i,node(i) + 1))
      neighbour <- nodeValid(shift) if neighbour != node
    } yield (node,neighbour)
    Graph(variables.elements.toSet, edges.map{case (n1,n2) => Set(n1,n2)}(collection.breakOut))
  }

  def complete(n: Int): Graph[Int] = {
    val nodes = 0 until n
    val edges = for(n1 <- 0 until n; n2 <- (n1 + 1) until n) yield Set(n1,n2)
    Graph(nodes.toSet, edges.toSet)
  }
  def unconnected(nodes: Int): Graph[Int] = Graph(Set(0 until nodes:_*), Set())
  def singleCycle(nodes: Int): Graph[Int] = lattice(nodes -> true).map(_.head)
}

/** Attaches domain sizes to the nodes of a hyper graph. */
case class StructureGenerator[N](graphGen: Generator[Graph[N]], domainGenerator: N => Generator[Int])
  extends Generator[LabeledProblemStructure[N]] {

  override def generate(r: Random): LabeledProblemStructure[N] = {
    val graph = graphGen.generate(r)
    val nodes = new SIIndex(graph.nodes)
    val domains: Array[Int] = nodes.elements.map(domainGenerator).map(_.generate(r))(collection.breakOut)
    val structure = StructureOnly(domains,graph.edges.map(_.map(nodes.forward)(collection.breakOut): Array[Int])(collection.breakOut))
    LabeledProblemStructure(structure,nodes)
  }
}

object StructureGenerator {
  def fixedDomainSize[N](graphGen: Generator[Graph[N]], domainSize: Int): StructureGenerator[N] =
    StructureGenerator(graphGen,_ => Constant(domainSize))
}

case class LabeledProblem[N](problem: Problem, variableLabels: Index[N])
