package vultura.util.graph

case class EdgeMapDiGraph[N](nodes: Set[N], diSuccessors: Map[N,Set[N]]){
  val diSuccDef = diSuccessors.withDefaultValue(Set())
  def edges: Set[(N,N)] = (for((src, dests) <- diSuccessors; dest <- dests) yield src -> dest)(collection.breakOut)
  def toBiMap = EdgeBiMapDiGraph(nodes,diSuccessors,edges.groupBy(_._2).map{case (dst,edges) => dst -> edges.map(_._1).toSet})
  /** @return all successor nodes to `n`, excluding `n` unless it participates in a cycle. */
  def tranSuccessors(n: N): Set[N] = {
    def succs(fringe: Set[N],acc: Set[N] = Set()): Set[N] =
      if(fringe.isEmpty) acc else succs(fringe.flatMap(diSuccDef).filterNot(acc ++ fringe),acc ++ fringe)
    succs(diSuccDef(n))
  }
  def isAcyclic = nodes.forall(n => !tranSuccessors(n).contains(n))
  def findCycle: Option[Seq[N]] = nodes.view.map(n => shortestPath(n,n)).collectFirst{case Some(cycle) => cycle}
  def shortestPath(start: N, goal: N): Option[Seq[N]] = {
    def find(fringe: List[List[N]], closed: Set[N] = Set()): Option[List[N]] = fringe match {
      case Nil => None
      case (p@(g :: _)) :: _ if g == goal => Some(p.reverse)
      case (exp@(next :: _)) :: rest if !closed(next) => find(rest ::: diSuccDef(next).toList.map(_ :: exp),closed + next)
      case _ :: rest => find(rest,closed)
    }
    find(diSuccDef(start).toList.map(_ :: start :: Nil),Set(start))
  }

  def subgraph(p: N => Boolean): EdgeMapDiGraph[N] =
    EdgeMapDiGraph(nodes.filter(p),edges.filter{case (a,b) => p(a) && p(b)})
}

object EdgeMapDiGraph{
  def calculateForwardMap[N](edges: Set[(N,N)]): Map[N,Set[N]] = edges.groupBy(_._1).map{case (x,ys) => x -> ys.map(_._2).toSet}
  def calculateBackwardMap[N](edges: Set[(N,N)]): Map[N,Set[N]] = edges.groupBy(_._2).map{case (x,ys) => x -> ys.map(_._1).toSet}
  def apply[N](nodes:Set[N], edges: Set[(N,N)]): EdgeMapDiGraph[N] = EdgeMapDiGraph(nodes,calculateForwardMap(edges))
}

case class EdgeBiMapDiGraph[N](nodes: Set[N], successors: Map[N,Set[N]], predecessors: Map[N,Set[N]]){
  val succDef = successors.withDefaultValue(Set())
  val predDef = predecessors.withDefaultValue(Set())

  def undirectedComponent(root: N): Set[N] = {
    def findComponent(closed: Set[N],fringe: Set[N]): Set[N] =
      if(fringe.isEmpty) closed
      else {
        val newClosed = closed ++ fringe
        findComponent(
          newClosed,
          for(x <- fringe; y <- succDef(x) ++ predDef(x) if !newClosed.contains(y)) yield y)
      }

    findComponent(Set(),Set(root))
  }

  def components: Set[Set[N]] = {
    Iterator.iterate((nodes,Set[Set[N]]())){ case x@(remaining,acc) =>
      if(remaining.isEmpty) x
      else {
        val next = remaining.head
        val comp = undirectedComponent(next)
        (remaining -- comp,acc + comp)
      }
    }.dropWhile(!_._1.isEmpty).next()._2
  }
}