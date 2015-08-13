package vultura.util.graph

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.Random

case class EdgeMapDiGraph[N](nodes: Set[N], successorsMap: Map[N,Set[N]]){
  val successors = successorsMap.withDefaultValue(Set())
  def edges: Set[(N,N)] = (for((src, dests) <- successors; dest <- dests) yield src -> dest)(collection.breakOut)
  def toBiMap = EdgeBiMapDiGraph(nodes,successors,edges.groupBy(_._2).map{case (dst,edges) => dst -> edges.map(_._1)})
  
  /** @return all successor nodes to `n`, excluding `n` unless it participates in a cycle. */
  def tranSuccessors(n: N): Set[N] = {
    def succs(fringe: Set[N],acc: Set[N] = Set()): Set[N] =
      if(fringe.isEmpty) acc else succs(fringe.flatMap(successors).filterNot(acc ++ fringe),acc ++ fringe)
    succs(successors(n))
  }
  
  lazy val isAcyclic = scc.values.toSet == nodes
  
  def findCycle: Option[Seq[N]] = nodes.view.map(n => shortestPath(n,n)).collectFirst{case Some(cycle) => cycle}
  def shortestPath(start: N, goal: N): Option[Seq[N]] = {
    def find(fringe: Queue[List[N]], closed: Set[N] = Set()): Option[List[N]] = if(fringe.isEmpty) None else
      fringe.dequeue match {
        case (p@(g :: _),_) if g == goal => Some(p.reverse) //found goal
        case (exp@(next :: _), rest) if !closed(next) => find(rest enqueue successors(next).toList.map(_ :: exp),closed + next)
        case (_,rest) => find(rest,closed)
      }
    find(Queue(successors(start).toSeq.map(_ :: start :: Nil):_*),Set(start))
  }

  def subgraph(p: N => Boolean): EdgeMapDiGraph[N] =
    EdgeMapDiGraph(nodes.filter(p),edges.filter{case (a,b) => p(a) && p(b)})

  /** Strongly connected components. */
  lazy val scc: Map[N,N] = {
    //`dfs` finds all strongly connected components below `node`
    //`path` holds the the depth for all nodes above the current one
    //'sccs' holds the representatives found so far; the accumulator
    def dfs(node: N, path: Map[N,Int], sccs: Map[N,N]): Map[N,N] = {
      //returns the earliest encountered node of both arguments
      //for the case both aren't on the path, `old` is returned
      def shallowerNode(old: N,candidate: N): N =
        (path.get(old),path.get(candidate)) match {
          case (_,None) => old
          case (None,_) => candidate
          case (Some(dOld),Some(dCand)) =>  if(dCand < dOld) candidate else old
        }

      //handle the child nodes
      val children: Set[N] = successors(node)
      //the initially known shallowest back-link is `node` itself
      val (newState,shallowestBackNode) = children.foldLeft((sccs,node)){
        case ((foldedSCCs,shallowest),child) =>
          if(path.contains(child))
            (foldedSCCs, shallowerNode(shallowest,child))
          else {
            val sccWithChildData = dfs(child,path + (node -> path.size),foldedSCCs)
            val shallowestForChild = sccWithChildData(child)
            (sccWithChildData, shallowerNode(shallowest, shallowestForChild))
          }
      }

      newState + (node -> shallowestBackNode)
    }

    //run the above function, so every node gets visited
    nodes.foldLeft(Map[N,N]()){ case (sccs,nextNode) =>
      if(sccs.contains(nextNode))
        sccs
      else
        dfs(nextNode,Map(),sccs)
    }
  }
  
  /** Tests whether the set of node-subsets is a node cover, and each induced subgraph is acyclic. */
  def isDAGCover(dagc: Set[Set[N]]): Boolean = {
    val isCover = dagc.flatten == nodes
    val isAcyclic = dagc.forall(c => subgraph(c).isAcyclic)
    isCover && isAcyclic
  }

  /** Check that there is no node that can be added to the cover while it remains uncyclic. */
  def isMinimalAcyclicSubgraph(sg: Set[N]): Boolean = nodes.forall(n => sg(n) || !subgraph(sg + n).isAcyclic)
  
  def computeDAGCover(r: Random): Set[Set[N]] = {
    import vultura.util._
    val hits: mutable.HashMap[N,Int] = nodes.map(_ -> 0)(collection.breakOut)

    /**
     * @param picked Already included in the cover to be built.
     * @param pickSuccessors Nodes that are successors to picked nodes. These cannot be included if they have a picked successor.
     *                       If this is detected, they get removed.
     * @param remaining Nodes that can still be picked and have not been included in pickSuccessors.
     */
    @tailrec
    def buildCover(picked: Set[N], pickSuccessors: Set[N], remaining: Set[N]): Set[N] = {
      val activeSuccs = pickSuccessors.filter(ps => successors(ps).forall(!picked(_)))
      val candidates = activeSuccs ++ remaining -- picked
      if(candidates.isEmpty)
        picked
      else {
        val newPick = vultura.util.maxByMultiple(candidates)(c => -hits(c)).pickRandom(r)
        val newSuccessors: Set[N] = successors(newPick) -- picked
        buildCover(picked + newPick, activeSuccs ++ newSuccessors, remaining -- newSuccessors - newPick)
      }
    }

    val result = Set.newBuilder[Set[N]]
    while(hits.values.min == 0){
      val nextDAG: Set[N] = buildCover(Set(), Set(), nodes)
      result += nextDAG
      //increment hitting set
      nextDAG.foreach(n =>
        hits(n) = hits(n) + 1
      )
    }
    result.result()
  }
}

object EdgeMapDiGraph{
  def calculateForwardMap[N](edges: Set[(N,N)]): Map[N,Set[N]] = edges.groupBy(_._1).map{case (x,ys) => x -> ys.map(_._2)}
  def calculateBackwardMap[N](edges: Set[(N,N)]): Map[N,Set[N]] = edges.groupBy(_._2).map{case (x,ys) => x -> ys.map(_._1)}
  def apply[N](nodes:Set[N], edges: Set[(N,N)]): EdgeMapDiGraph[N] = new EdgeMapDiGraph(nodes,calculateForwardMap(edges).withDefaultValue(Set()))

  def erdosRenyi(numNodes: Int, edgeProb: Double, r: Random) =  {
    val nodes: Set[Int] = (1 to numNodes).toSet
    val edges = for{
      n1 <- nodes
      n2 <- nodes if r.nextDouble() < edgeProb && n2 != n1
    } yield n1 -> n2
    EdgeMapDiGraph(nodes,edges)
  }
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
    }.dropWhile(_._1.nonEmpty).next()._2
  }
}