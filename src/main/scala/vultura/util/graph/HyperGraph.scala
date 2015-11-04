package vultura.util.graph

/** Type-class for hyper graphs. Edges are represented as sets of nodes.
  * @tparam X This class represents the hyper-graph.
  * @tparam N The node type. */
trait HyperGraph[-X,N] {
  type E = Set[N]
  def nodes(x: X): Set[N]
  def incidentEdges(x: X, n: N): Set[E]
  def edges(x: X): Set[E] = nodes(x).flatMap(incidentEdges(x,_))
}

/** Type-class for undirected graphs.
  *
  * @tparam X
  * @tparam N
  */
trait UndirectedGraph[-X,N] {
  def nodes(x: X): Set[N]
  def neighbours(x: X, n: N): Set[N]
}

object UndirectedGraph {
  def fromDirectedGraph[X,N,E](implicit dg: IsDirectedGraph[X,N]): UndirectedGraph[X,N] = new UndirectedGraph[X,N] {
    override def nodes(x: X): Set[N] = dg.nodes(x)
    override def neighbours(x: X, n: N): Set[N] =
      dg.children(x,n) ++ dg.nodes(x).filter(dg.children(x,_).contains(n))
  }
}
