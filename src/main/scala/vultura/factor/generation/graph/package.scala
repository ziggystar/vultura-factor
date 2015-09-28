package vultura.factor.generation

import vultura.factor._
import vultura.util.{IntDomainCPI, SIIndex}

/** Generation of (random) undirected graph structures. */
package object graph {
  /** Build a n-dimensional lattice.
    * First in tuple is dimension size, second is toroidal condition on this dimension (true means wrapping). */
  def lattice(dimensions: (Int,Boolean)*): Graph[IndexedSeq[Int]] = {
    val variables = new SIIndex[IndexedSeq[Int]](IntDomainCPI(dimensions.map(_._1).map(0 until).map(_.toArray).toArray).seq.map(_.toIndexedSeq))
    def nodeValid(coords: IndexedSeq[Int]): Option[IndexedSeq[Int]] = {
      val fixed = coords.zip(dimensions).map {
        case (xi, (di, _)) if xi >= 0 && xi < di => Some(xi)
        case (xi, (di, true)) if xi == -1 => Some(di - 1)
        case (xi, (di, true)) if xi == di => Some(0)
        case _ => None
      }
      val filtered = fixed.flatten
      if(filtered.length == fixed.length) Some(filtered)
      else None
    }
    val edges: IndexedSeq[(IndexedSeq[Var], IndexedSeq[Var])] = for {
      node <- variables.elements
      shift <- dimensions.indices.map(i => node.updated(i,node(i) + 1))
      neighbour <- nodeValid(shift) if neighbour != node
    } yield (node,neighbour)
    Graph(variables.elements.toSet, edges.map{case (n1,n2) => Set(n1,n2)}(collection.breakOut))
  }

  def erdoesRenyi(nodes: Int, p: Double): Generator[Graph[Int]] = ???

  def complete(n: Int): Graph[Int] = {
    val nodes = 0 until n
    val edges = for(n1 <- 0 until n; n2 <- (n1 + 1) until n) yield Set(n1,n2)
    Graph(nodes.toSet, edges.toSet)
  }
  def unconnected(nodes: Int): Graph[Int] = Graph(Set(0 until nodes:_*), Set())
  def singleCycle(nodes: Int): Graph[Int] = lattice(nodes -> true).map(_.head)
}