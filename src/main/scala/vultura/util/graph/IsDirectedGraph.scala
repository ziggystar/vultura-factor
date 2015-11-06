package vultura.util.graph

import scala.annotation.tailrec

/** Type-class for directed graphs with nodes of type `N`.
  *
  * @tparam X This type represents the directed graph.
  * @tparam N Type of the nodes.
  */
trait IsDirectedGraph[-X,N] {
  def nodes(x: X): Set[N]
  def edges(x: X): Set[(N,N)]
  def children(x: X, node: N): Set[N] = (for((p,c) <- edges(x) if p == node) yield c)(collection.breakOut)
}

/** Implementing this minimal directed graph interface provides a IsDirectedGraph type-class instance automatically. */
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

/** This trait defines the operations available on directed graphs with nodes of type `N`. */
trait DiGraphOps[N] extends DiGraph[N] {
  def nodes: Set[N]
  def edges: Set[(N,N)]
  def children(node: N): Set[N]
  def filter(pnodes: N => Boolean, pedges: ((N,N)) => Boolean): DiGraphOps[N]
  /** Find topologically ordered strongly connected components of the graph. */
  def tarjanSCC: List[Set[N]]
  /** Partition the graph into a set of (weakly) connected components, this means that arrow direction is ignored.
    * @return A set of components (each a set of vertices). Each component is guaranteed to be non-empty. */
  def connectedComponents: Set[Set[N]]
  /** Reverse the edges of the graph. */
  def transpose: DiGraphOps[N]
  def ancestors(node: N): Set[N]
  def parents(node: N): Set[N]
  def descendants(node: N): Set[N]
  def isAcyclic: Boolean
  def isTree: Boolean
  def filterNodes(p: N => Boolean): DiGraphOps[N] = filter(p, _ => true)
  def filterEdges(p: ((N,N)) => Boolean): DiGraphOps[N] = filter(_ => true, p)
  def graphEqual[X](other: X)(implicit dg: IsDirectedGraph[X,N])
}

/** This trait implements operations of [[vultura.util.graph.DiGraphOps]] rather inefficiently.
  * For an efficient implementation see [[vultura.util.graph.LabeledGraph]].*/
trait DiGraphInstOps[N] extends DiGraphOps[N] {
  type G
  def instance: G
  def typeClass: IsDirectedGraph[G,N]

  def nodes: Set[N] = typeClass.nodes(instance)
  def edges: Set[(N,N)] = typeClass.edges(instance)
  def children(node: N): Set[N] = typeClass.children(instance, node)

  lazy val lGraph = LabeledGraph(instance)(typeClass)

  def filter(pnodes: N => Boolean, pedges: ((N,N)) => Boolean): DiGraphOps[N] = lGraph.filter(pnodes,pedges)

  /** Find topologically ordered strongly connected components of the graph. */
  def tarjanSCC: List[Set[N]] = lGraph.tarjanSCC

  /** Reverse the edges of the graph. */
  def transpose: DiGraphOps[N] = lGraph.transpose
  def ancestors(node: N): Set[N] = transpose.descendants(node)

  def parents(node: N): Set[N] = for((p,n) <- edges if n == node) yield p
  def descendants(node: N): Set[N] = Iterator.iterate(children(node)){ fringe =>
    fringe.flatMap(children) ++ fringe
  }.sliding(2).dropWhile(ss => ss(0) != ss(1)).next().head
  def isAcyclic: Boolean = tarjanSCC.forall(_.size == 1)
  def isTree: Boolean = connectedComponents.forall{c =>
    //number of edges must be n-1
    edges.count{case (n1,n2) => c(n1) && c(n2)} == c.size - 1
  }
  /** Partition the graph into a set of (weakly) connected components, this means that arrow direction is ignored.
    * @return A set of components (each a set of vertices). Each component is guaranteed to be non-empty. */
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
