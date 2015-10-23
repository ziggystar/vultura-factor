package vultura.factor.generation

import vultura.factor.StructureOnly
import vultura.factor.generation.graph.Graph
import vultura.util.SIIndex

/** Adds domain sizes to an `N`-labeled [[vultura.factor.generation.graph.Graph]] to obtain a labeled problem structure. */
trait Domainification[-N]{
  def addDomains[L <: N](graph: Graph[L]): Generator[LabeledProblemStructure[L]]
}

/** i.i.d. domains size for all variables. */
case class IIDDomainSize(ds: Generator[Int]) extends Domainification[Any] {
  override def addDomains[L <: Any](graph: Graph[L]): Generator[LabeledProblemStructure[L]] = {
    val nodes = new SIIndex(graph.nodes)
    val domains: Generator[Array[Int]] = Generator.seq(Seq.fill(nodes.size)(ds)).map(_.toArray)
    domains.map(doms =>
      LabeledProblemStructure(StructureOnly(doms,graph.edges.map(_.map(nodes.forward)(collection.breakOut): Array[Int])(collection.breakOut)),nodes)
    )
  }
}

/** Constant domain size for all variables. */
case class FixedDomainsSize(n: Int = 2) extends Domainification[Any]{
  val inner = IIDDomainSize(Constant(n))
  override def addDomains[L <: Any](graph: Graph[L]): Generator[LabeledProblemStructure[L]] = inner.addDomains(graph)
}