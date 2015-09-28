package vultura.factor.generation.graph

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
