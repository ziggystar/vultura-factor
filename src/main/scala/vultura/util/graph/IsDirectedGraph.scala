package vultura.util.graph

import vultura.util.{Index, SIIndex}

import scala.collection.mutable

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
  def parents(node: N): Set[N]
  def ancestors(node: N): Set[N]
  def descendants(node: N): Set[N]
  /** Find topologically ordered strongly connected components of the graph. */
  def tarjanSCC: List[Set[N]]
  /** Reverse the edges of the graph. */
  def transpose: DiGraphOps[N]
  def isAcyclic: Boolean = tarjanSCC.forall(_.size == 1)
  def filter(nodes: N => Boolean, edges: ((N,N)) => Boolean): DiGraphOps[N]
  def filterNodes(p: N => Boolean): DiGraphOps[N] = filter(p,_ => true)
  def filterEdges(p: ((N,N)) => Boolean): DiGraphOps[N] = filter(_ => true, p)
  def graphEqual[X](other: X)(implicit dg: IsDirectedGraph[X,N]) = nodes == dg.nodes(other) && edges == dg.edges(other)
}

object IsDirectedGraph {
  /** This class provides operations for types that have a IsDirectedGraph instance.
    * Many operations are **very** inefficient. */
  case class DiGraphWrapper[X,N](x: X)(implicit dg: IsDirectedGraph[X,N]) extends DiGraphOps[N] { outer =>
    override def nodes: Set[N] = dg.nodes(x)
    override def edges: Set[(N, N)] = dg.edges(x)
    override def children(node: N): Set[N] = dg.children(x, node)
    override def parents(node: N): Set[N] = for((p,n) <- edges if n == node) yield p
    override def descendants(node: N): Set[N] = Iterator.iterate(children(node)){ fringe =>
      fringe.flatMap(children) ++ fringe
    }.sliding(2).dropWhile(ss => ss(0) != ss(1)).next().head
    override def ancestors(node: N): Set[N] = transpose.descendants(node)
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

/** An efficient directed graph implementation that uses one array per node for representing its children. */
class LabeledGraph[N] protected[LabeledGraph](val index: Index[N], protected val childs: Array[Array[Int]]) extends DiGraphOps[N] {

  object intGraph {
    val numNodes: Int = childs.length
    val nodeRange: Range = 0 until numNodes

    lazy val pars: Array[Array[Int]] = {
      val resultHolder = IndexedSeq.fill(numNodes)(mutable.Set.newBuilder[Int])
      for(pi <- nodeRange; ci <- childs(pi)) {
        resultHolder(ci) += pi
      }
      resultHolder.map(_.result().toArray)(collection.breakOut)
    }

    def nodes: Set[Int] = nodeRange.toSet
    def children(node: Int): Set[Int] = childs(node).toSet
    def parents(node: Int): Set[Int] = pars(node).toSet
    /** Find topologically ordered strongly connected components of the graph. */
    def edges: Set[(Int, Int)] = for(p <- nodes; c <- children(p)) yield p -> c
    /** Reverse the edges of the graph. */
    def descendants(node: Int): Set[Int] = searchAll(children(node),Set(),childs)
    def ancestors(node: Int): Set[Int] = searchAll(children(node),Set(),pars)

    def searchAll(fringe: Set[Int], closed: Set[Int], succ: Array[Array[Int]]): Set[Int] = {
      val newNodes = fringe.flatMap(succ(_)) -- closed
      if(newNodes.isEmpty)
        closed ++ fringe
      else
        searchAll(newNodes, closed ++ fringe, succ)
    }

    lazy val tarjanSCC = {
      val tj_index = Array.fill[Int](numNodes)(-1)
      val lowLink = new Array[Int](numNodes)
      val stack = new mutable.Stack[Int]()
      val onStack = new mutable.HashSet[Int]()

      var sccs: List[Set[Int]] = Nil

      var nextIndex = 0

      def strongConnect(v: Int): Unit = {
        tj_index(v) = nextIndex
        lowLink(v) = nextIndex
        nextIndex += 1
        stack.push(v)
        onStack += v

        //Consider successors of v
        for (w <- childs(v)) {
          if (tj_index(w) == -1) {
            strongConnect(w)
            lowLink(v) = math.min(lowLink(v), lowLink(w))
          } else if (onStack(w)) {
            lowLink(v) = math.min(lowLink(v), tj_index(w))
          }
        }

        if (lowLink(v) == tj_index(v)) {
          //start new scc
          var newComponent: List[Int] = Nil
          var w = 0
          do {
            w = stack.pop()
            onStack -= w
            newComponent = w :: newComponent
          } while (w != v)
          sccs = newComponent.toSet :: sccs
        }
      }

      for (v <- tj_index.indices) {
        if (tj_index(v) == -1) strongConnect(v)
      }

      sccs.reverse
    }
  }
  lazy val nodes: Set[N] = index.elements.toSet
  lazy val edges: Set[(N, N)] = intGraph.edges.map{case (i1,i2) => index.backward(i1) -> index.backward(i2)}
  override def children(node: N): Set[N] = intGraph.children(index.forward(node)).map(index.backward)
  override def descendants(node: N): Set[N] = intGraph.descendants(index.forward(node)).map(index.backward)
  override def ancestors(node: N): Set[N] = intGraph.ancestors(index.forward(node)).map(index.backward)
  override def parents(node: N): Set[N] = intGraph.parents(index.forward(node)).map(index.backward)
  override def tarjanSCC: List[Set[N]] = intGraph.tarjanSCC.map(_.map(index.backward))
  /** Reverse the edges of the graph. */
  override def transpose: LabeledGraph[N] = LabeledGraph.fromChildList(nodes, parents)
  override def filter(nodeP: (N) => Boolean, edgeP: ((N, N)) => Boolean): LabeledGraph[N] = {
    val newNodes: Set[N] = nodes.filter(nodeP)
    LabeledGraph.fromChildList(
      newNodes,
      newNodes.map(n => n -> children(n).filter(c => edgeP((n,c))))(collection.breakOut): Map[N,Iterable[N]])
  }
}

object LabeledGraph {
  def apply[X,N](x: X)(implicit dg: IsDirectedGraph[X,N]): LabeledGraph[N] = {
    val index = new SIIndex[N](dg.nodes(x))
    new LabeledGraph[N](
      index,
      index.elements.map(n => dg.children(x,n).map(index.forward)(collection.breakOut):Array[Int])(collection.breakOut))
  }
  def fromChildList[N](nodes: Iterable[N], children: N => Iterable[N]) = {
    val index = new SIIndex(nodes)
    new LabeledGraph[N](
      index,
      index.elements.map(n => children(n).map(index.forward)(collection.breakOut):Array[Int])(collection.breakOut))
  }
}

/** Import for type-class instances for maps. */
object TupleSeqsAreGraphs {
  implicit def tupleSeqGraphInstance[N]: IsDirectedGraph[Iterable[(N,N)],N] = new IsDirectedGraph[Iterable[(N,N)],N] {
    override def nodes(x: Iterable[(N,N)]): Set[N] = (x.map(_._1)(collection.breakOut): Set[N]) ++ x.map(_._2)
    override def edges(x: Iterable[(N,N)]): Set[(N, N)] = x.toSet
  }
}