package vultura.util.graph

import vultura.util.{SIIndex, Index}

import scala.collection.mutable

/** An efficient directed graph implementation that uses one array per node for representing its children. */
class LabeledGraph[N] protected[LabeledGraph](val index: Index[N], protected val childs: Array[Array[Int]])
  extends DiGraphOps[N] { outer =>

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
    def ancestors(node: Int): Set[Int] = searchAll(parents(node),Set(),pars)

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
  override lazy val nodes: Set[N] = index.elements.toSet
  override lazy val edges: Set[(N, N)] = intGraph.edges.map{case (i1,i2) => index.backward(i1) -> index.backward(i2)}
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
      newNodes.map(n => n -> children(n).filter(c => nodeP(n) && nodeP(c) && edgeP((n,c))))(collection.breakOut): Map[N,Iterable[N]])
  }

  val instGraph = new DiGraphInstOps[N] {
    override type G = outer.type
    override def typeClass: IsDirectedGraph[G, N] = new IsDirectedGraph[G,N] {
      override def nodes(x: outer.type): Set[N] = outer.nodes
      override def edges(x: outer.type): Set[(N, N)] = outer.edges
    }
    override def instance: G = outer
  }
  override def isAcyclic: Boolean = instGraph.isAcyclic
  override def graphEqual[X](other: X)(implicit dg: IsDirectedGraph[X, N]): Unit = instGraph.graphEqual(other)
  override def isTree: Boolean = instGraph.isTree

  /** Partition the graph into a set of (weakly) connected components, this means that arrow direction is ignored.
    * @return A set of components (each a set of vertices). Each component is guaranteed to be non-empty. */
  override def connectedComponents: Set[Set[N]] = instGraph.connectedComponents
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
