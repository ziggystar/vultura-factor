package vultura.util.graph2

import vultura.util.{Index, SIIndex}

import scala.collection.mutable

/** Type-class for directed graphs.
  *
  * @tparam X
  * @tparam N
  */
trait DirectedGraph[-X,N] {
  def nodes(x: X): Set[N]
  def edges(x: X): Set[(N,N)]
  def children(x: X, node: N): Set[N] = (for((p,c) <- edges(x) if p == node) yield c)(collection.breakOut)
  def parents(x: X, node: N): Set[N] = (for((p,c) <- edges(x) if c == node) yield p)(collection.breakOut)
}

object DirectedGraph{
  case class BiMapGraph[N] protected[DirectedGraph](nodes: Set[N], children: Map[N,Set[N]], parents: Map[N,Set[N]]) {
    def filterNodes(p: N => Boolean): BiMapGraph[N] = new BiMapGraph[N](nodes.filter(p),
      for((parent,cs) <- children if p(parent)) yield parent -> cs.filter(p),
      for((child,ps) <- parents if p(child)) yield child-> ps.filter(p)
    )

    def edges: Set[(N,N)] = (for((p,cs) <- children; c <- cs) yield (p,c))(collection.breakOut)
  }

  def materialize[X,N](x: X)(implicit dg: DirectedGraph[X,N]): BiMapGraph[N] = {
    val nodes = dg.nodes(x)
    val parentBuilder = new mutable.HashMap[N,Set[N]]().withDefaultValue(Set())
    val childrenBuilder = new mutable.HashMap[N,Set[N]]().withDefaultValue(Set())
    dg.edges(x).foreach{ case (p,c) =>
        parentBuilder.update(c,parentBuilder(c) + p)
        childrenBuilder.update(p,childrenBuilder(p) + c)
    }
    new BiMapGraph[N](nodes, childrenBuilder.toMap.withDefaultValue(Set()), parentBuilder.toMap.withDefaultValue(Set()))
  }

  def fastGraph[X,N](x: X)(implicit dg: DirectedGraph[X,N]): (IntGraph,Index[N]) = {
    val nodes = new SIIndex[N](dg.nodes(x))
    val g = new IntGraph(nodes.size, nodes.indices.map(in => dg.children(x,nodes.backward(in)).map(nodes(_))(collection.breakOut): Array[Int])(collection.breakOut))
    (g,nodes)
  }

  implicit def biMapGInstance[N] = new DirectedGraph[BiMapGraph[N],N] {
    override def nodes(x: BiMapGraph[N]): Set[N] = x.nodes
    override def children(x: BiMapGraph[N], node: N): Set[N] = x.children(node)
    override def edges(x: BiMapGraph[N]): Set[(N, N)] = x.edges
    override def parents(x: BiMapGraph[N], node: N): Set[N] = x.parents(node)
  }
}

case class ChildList[N](nodes: Set[N], children: Map[N,Set[N]])
object ChildList {
  implicit def childListDGInstance[N] = new DirectedGraph[ChildList[N],N]{
    override def nodes(x: ChildList[N]): Set[N] = x.nodes
    override def edges(x: ChildList[N]): Set[(N, N)] =
      (for ((p, chs) <- x.children; c <- chs) yield (p, c))(collection.breakOut)
    override def children(x: ChildList[N], node: N): Set[N] = x.children.getOrElse(node,Set())
  }
}

/** Import for type-class instances for maps. */
object TupleSeqsAreGraphs {
  implicit def tupleSeqGraphInstance[N]: DirectedGraph[Iterable[(N,N)],N] = new DirectedGraph[Iterable[(N,N)],N] {
    override def nodes(x: Iterable[(N,N)]): Set[N] = (x.map(_._1)(collection.breakOut): Set[N]) ++ x.map(_._2)
    override def edges(x: Iterable[(N,N)]): Set[(N, N)] = x.toSet
  }
}

case class IntGraph protected[graph2](numNodes: Int, children: Array[Array[Int]]) {
  val nodes: Range = 0 until numNodes

  /** Produces the connected components of this graph in topological order. */
  def tarjanSCC: List[Set[Int]] = {
    val g = this
    val index = Array.fill[Int](g.nodes.size)(-1)
    val lowLink = new Array[Int](g.nodes.size)
    val stack = new mutable.Stack[Int]()
    val onStack = new mutable.HashSet[Int]()

    var sccs: List[Set[Int]] = Nil

    var nextIndex = 0

    def strongConnect(v: Int): Unit = {
      index(v) = nextIndex
      lowLink(v) = nextIndex
      nextIndex += 1
      stack.push(v)
      onStack += v

      //Consider successors of v
      for (w <- g.children(v)) {
        if (index(w) == -1) {
          strongConnect(w)
          lowLink(v) = math.min(lowLink(v), lowLink(w))
        } else if (onStack(w)) {
          lowLink(v) = math.min(lowLink(v), index(w))
        }
      }

      if (lowLink(v) == index(v)) {
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

    for (v <- g.nodes) {
      if (index(v) == -1) strongConnect(v)
    }

    sccs.reverse
  }
}