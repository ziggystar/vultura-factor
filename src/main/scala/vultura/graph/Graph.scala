package vultura.graph


/**
 * Type class for undirected graphs whose nodes are of type A.
 *
 * User: Thomas Geier
 * Date: 23.09.11
 */

trait Graph[G,A] {
  def nodes(g: G): Set[A]
  def adjacent(g: G, n1: A, n2: A): Boolean
  def edges(g: G): Set[(A,A)] = for(
    n1 <- nodes(g); n2 <- nodes(g) if(adjacent(g,n1,n2));
    t = (n1,n2)
  ) yield t
  def neighbours(g: G, v: A): Set[A] = for(e <- edges(g) if(e._1 == v); n = e._2) yield n
}

/** warning: cannot represent unconnected edges */
object TupleSetGraph{
  implicit def ts2graph[A]: Graph[Set[(A,A)],A] = new Graph[Set[(A,A)],A] {
    def nodes(g: Set[(A, A)]): Set[A] = g flatMap (t => Set(t._1,t._2))
    def adjacent(g: Set[(A, A)], n1: A, n2: A): Boolean = g.contains((n1,n2)) || g.contains((n2,n1))
  }
}

object AdjacencyMapGraph {
  type SL[A] = Map[A, Set[A]]
  implicit def al2graph[A]: Graph[SL[A],A] = new Graph[SL[A],A] {
    def nodes(g: SL[A]): Set[A] = g.keySet
    def adjacent(g: SL[A], n1: A, n2: A): Boolean = g(n1).contains(n2) || g(n2).contains(n1)
  }
}

object AdjacencyListGraph {
  type SL[A] = (Set[A],Iterable[(A,A)])
  implicit def al2graph[A]: Graph[SL[A],A] = new Graph[SL[A],A] {
    def nodes(g: (Set[A], Iterable[(A, A)])): Set[A] = g._1
    def adjacent(g: (Set[A], Iterable[(A, A)]), n1: A, n2: A): Boolean = g._2.exists{adj => adj == (n1,n2) || adj == (n2,n1)}
  }
}

object SetSetGraph{
  implicit def ss2graph[A]: Graph[Set[Set[A]],Set[A]] = new Graph[Set[Set[A]],Set[A]] {
    def nodes(g: Set[Set[A]]): Set[Set[A]] = g
    def adjacent(g: Set[Set[A]], n1: Set[A], n2: Set[A]): Boolean = n1 exists n2
  }
}