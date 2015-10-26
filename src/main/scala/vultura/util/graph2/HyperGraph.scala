package vultura.util.graph2

/** Type-class for hyper graphs. Edges are represented as sets of nodes.
  * @tparam X This class represents the hyper-graph.
  * @tparam N The node type. */
trait HyperGraph[X,N] {
  type E = Set[N]
  def nodes(x: X): Set[N]
  def incidentEdges(x: X, n: N): Set[E]
  def edges(x: X): Set[E] = nodes(x).flatMap(incidentEdges(x,_))
}

/** Type-class for directed graphs.
  *
  * @tparam X
  * @tparam N
  * @tparam E
  */
trait DirectedGraph[X,N,E] {
  def nodes(x: X): Set[N]
  def successors(x: X, node: N): Set[E]
}

/** Type-class for undirected graphs.
  *
  * @tparam X
  * @tparam N
  * @tparam E
  */
trait UndirectedGraph[X,N,E] {
  def nodes(x: X): Set[N]
  def edges(x: X): Set[E]
  def incident(x: X, e: E): BiSet[N]
}

object UndirectedGraph {
  def fromDirectedGraph[X,N,E](implicit dg: DirectedGraph[X,N,E]): UndirectedGraph[X,N,E] = new UndirectedGraph[X,N,E] {
    override def nodes(x: X): Set[N] = dg.nodes(x)
    override def neighbours(x: X, node: N): Set[N] =
  }
}
