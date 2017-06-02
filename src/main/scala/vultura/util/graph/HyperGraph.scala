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
  * Has to implement one of neighbours or edges.
  * @tparam X
  * @tparam N
  */
trait IsUndirectedGraph[-X,N] {
  def nodes(x: X): Set[N]
  def neighbours(x: X, n: N): Set[N] = edges(x).collect{case bs if bs.contains(n) => bs.toSet}.flatten
  def edges(x: X): Set[BiSet[N]] = nodes(x).flatMap(n => neighbours(x,n).map(nn => BiSet(n,n)))(collection.breakOut)
}

object IsUndirectedGraph {
  def fromDirectedGraph[X,N,E](implicit dg: IsDirectedGraph[X,N]): IsUndirectedGraph[X,N] = new IsUndirectedGraph[X,N] {
    override def nodes(x: X): Set[N] = dg.nodes(x)
    override def neighbours(x: X, n: N): Set[N] =
      dg.children(x,n) ++ dg.nodes(x).filter(dg.children(x,_).contains(n))
  }
}
