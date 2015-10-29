package vultura.util.graph2

import vultura.util.{SIIndex, Index}

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

  case class IntGraph protected[DirectedGraph](nodes: Range, children: Array[Array[Int]])

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
    val intNodes: Range = 0 until nodes.size
    val g = new IntGraph(intNodes, intNodes.map(in => dg.children(x,nodes.backward(in)).map(nodes(_))(collection.breakOut): Array[Int])(collection.breakOut))
    (g,nodes)
  }

  implicit def biMapGInstance[N] = new DirectedGraph[BiMapGraph[N],N] {
    override def nodes(x: BiMapGraph[N]): Set[N] = x.nodes
    override def children(x: BiMapGraph[N], node: N): Set[N] = x.children(node)
    override def edges(x: BiMapGraph[N]): Set[(N, N)] = x.edges
    override def parents(x: BiMapGraph[N], node: N): Set[N] = x.parents(node)
  }
}

/** Import for type-class instances for maps. */
object TupleSeqsAreGraphs {
  implicit def tupleSeqGraphInstance[N]: DirectedGraph[Iterable[(N,N)],N] = new DirectedGraph[Iterable[(N,N)],N] {
    override def nodes(x: Iterable[(N,N)]): Set[N] = (x.map(_._1)(collection.breakOut): Set[N]) ++ x.map(_._2)
    override def edges(x: Iterable[(N,N)]): Set[(N, N)] = x.toSet
  }
}