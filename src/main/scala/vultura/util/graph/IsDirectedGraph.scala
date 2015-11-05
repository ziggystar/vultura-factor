package vultura.util.graph

import scala.annotation.tailrec

/** Type-class for directed graphs.
  *
  * @tparam X This type represents the directed graph.
  * @tparam N Type of the nodes.
  */
trait IsDirectedGraph[-X,N] {
  def nodes(x: X): Set[N]
  def edges(x: X): Set[(N,N)]
  def children(x: X, node: N): Set[N] = (for((p,c) <- edges(x) if p == node) yield c)(collection.breakOut)
}

/** Implementing this minimal directed graph interface provides a IsDirectedGraph type-class automatically. */
trait DiGraph[N] {
  def nodes: Set[N]
  def edges: Set[(N,N)]
}

object DiGraph {
  implicit def diGraphInstance[N] = new IsDirectedGraph[DiGraph[N],N] {
    override def nodes(x: DiGraph[N]): Set[N] = x.nodes
    override def edges(x: DiGraph[N]): Set[(N, N)] = x.edges
  }
}

trait DiGraphOps[N] extends DiGraph[N] {
  def nodes: Set[N]
  def edges: Set[(N,N)]
  def children(node: N): Set[N]
  def filter(nodes: N => Boolean, edges: ((N,N)) => Boolean): DiGraphOps[N]
  /** Find topologically ordered strongly connected components of the graph. */
  def tarjanSCC: List[Set[N]]
  /** Reverse the edges of the graph. */
  def transpose: DiGraphOps[N]
  def ancestors(node: N): Set[N] = transpose.descendants(node)

  def parents(node: N): Set[N] = for((p,n) <- edges if n == node) yield p
  def descendants(node: N): Set[N] = Iterator.iterate(children(node)){ fringe =>
    fringe.flatMap(children) ++ fringe
  }.sliding(2).dropWhile(ss => ss(0) != ss(1)).next().head
  def isAcyclic: Boolean = tarjanSCC.forall(_.size == 1)
  def filterNodes(p: N => Boolean): DiGraphOps[N] = filter(p,_ => true)
  def filterEdges(p: ((N,N)) => Boolean): DiGraphOps[N] = filter(_ => true, p)
  /** Partition the graph into a set of (weakly) connected components, this means that arrow direction is ignored. */
  def connectedComponents: Set[Set[N]] = {
    @tailrec def findOneCC(found: Set[N], remainingNodes: Set[N]): Set[N] = {
      val newCC = found ++ (found.flatMap(n => parents(n) ++ children(n)) intersect remainingNodes)
      if(newCC == found) found
      else findOneCC(newCC, remainingNodes)
    }
    @tailrec def cc(remainingNodes: Set[N], foundCCs: Set[Set[N]]): Set[Set[N]] =
      if(remainingNodes.isEmpty)
        foundCCs
      else {
        val nextCC = findOneCC(Set(remainingNodes.head), remainingNodes)
        cc(remainingNodes -- nextCC, foundCCs + nextCC)
      }
    cc(nodes,Set())
  }
  def graphEqual[X](other: X)(implicit dg: IsDirectedGraph[X,N]) = nodes == dg.nodes(other) && edges == dg.edges(other)
}

object IsDirectedGraph {
  /** This class provides operations for types that have a IsDirectedGraph instance.
    * Many operations are **very** inefficient. */
  case class DiGraphWrapper[X,N](x: X)(implicit dg: IsDirectedGraph[X,N]) extends DiGraphOps[N] { outer =>
    override def nodes: Set[N] = dg.nodes(x)
    override def edges: Set[(N, N)] = dg.edges(x)
    override def children(node: N): Set[N] = dg.children(x, node)
    override def filter(nodes: (N) => Boolean, edges: ((N, N)) => Boolean): DiGraphOps[N] =
      Filtered(nodes,edges).diGraphView
    /** Reverse the edges of the graph. */
    override def transpose: DiGraphOps[N] = new DiGraph[N]{
      override def nodes: Set[N] = outer.nodes
      override def edges: Set[(N, N)] = outer.edges.map(_.swap)
    }.diGraphView
    override def tarjanSCC: List[Set[N]] = x.diGraph.tarjanSCC
    case class Filtered(nodeFilter: N => Boolean,
                        edgeFilter: ((N,N)) => Boolean) extends DiGraph[N] {
      override def nodes: Set[N] = dg.nodes(x).filter(nodeFilter)
      override def edges: Set[(N, N)] = dg.edges(x).filter(e => nodeFilter(e._1) && nodeFilter(e._2) && edgeFilter(e))
    }
  }

  implicit class toGraphExtension[X,N](val x: X)(implicit dg: IsDirectedGraph[X,N]) {
    def diGraphView: DiGraphWrapper[X, N] = DiGraphWrapper(x)
    def diGraph: LabeledGraph[N] = LabeledGraph(x)
  }
}


/** Import for type-class instances for maps. */
object TupleSeqsAreGraphs {
  implicit def tupleSeqGraphInstance[N]: IsDirectedGraph[Iterable[(N,N)],N] = new IsDirectedGraph[Iterable[(N,N)],N] {
    override def nodes(x: Iterable[(N,N)]): Set[N] = (x.map(_._1)(collection.breakOut): Set[N]) ++ x.map(_._2)
    override def edges(x: Iterable[(N,N)]): Set[(N, N)] = x.toSet
  }
}