package vultura.util

import scala.annotation.tailrec
import scala.collection.mutable

package object graph2 {
  /** Start a DFS traversal of the graph at a given node. Return the visited nodes in post-order. */
  def dfsPostOrder[G,N](g: G, node: N, closed: Set[N] = Set())(implicit dg: DirectedGraph[G,N]): List[N] = {
    @tailrec def dfs(stack: List[N] = Nil, closedR: Set[N] = closed, closedStack: List[N] = Nil): List[N] = stack match {
      case n :: r =>
        val succs = dg.children(g,n) -- closedR - n
        if(succs.isEmpty){
          dfs(r.filterNot(_ == n), closedR + n, n :: closedStack)
        }
        else
          dfs(succs ++: stack, closedR + n, closedStack)
      case Nil => closedStack.reverse
    }
    dfs(node :: Nil)
  }

  def dfsPostOrderingForest[G,N](g: G)(implicit dg: DirectedGraph[G,N]): List[N] = {
    val nodes: Set[N] = dg.nodes(g)
    @tailrec def dfsRestart(closed: Set[N] = Set(), acc: List[List[N]] = Nil): List[List[N]] =
      (nodes -- closed).headOption match {
        case None => acc.reverse
        case Some(next) =>
          val dfs = dfsPostOrder(g, next, closed)
          println(s"node $next, dfs $dfs")
          dfsRestart(closed ++ dfs, dfs :: acc)
      }
    dfsRestart().flatten
  }

  /** Reverse the edges within a directed graph. */
  def transpose[G,N](g: G)(implicit df: DirectedGraph[G,N]) = {
    import TupleSeqsAreGraphs._
    DirectedGraph.materialize(df.edges(g).map(_.swap))
  }

  /** Find SSCs using Kosaraju's algorithm*/
  def stronglyConnectedComponents[G,N](g: G)(implicit dg: DirectedGraph[G,N]): Set[Set[N]] = {
    import DirectedGraph._
    val efficient: BiMapGraph[N] = materialize(g)
    val postOrder: List[N] = dfsPostOrderingForest(efficient).reverse
    val transposed = transpose(efficient)
    @tailrec def findComponents(remaining: List[N] = postOrder, closed: Set[N] = Set(), components: Set[Set[N]] = Set()): Set[Set[N]] = remaining match {
      case Nil => components
      case n :: tail =>
        val order: List[N] = dfsPostOrder(transposed, n, closed)
        val component = order.toSet
        findComponents(remaining.filterNot(component), closed ++ component, components + component)
    }

    findComponents(postOrder)
  }

  def isSameGraph[G1,G2,N](g: G1, g2: G2)(implicit dg1: DirectedGraph[G1,N], dg2: DirectedGraph[G2,N]): Boolean =
    dg1.nodes(g) == dg2.nodes(g2) && dg1.edges(g) == dg2.edges(g2)

  def tarjanSCC[G,N](_g: G)(implicit dg: DirectedGraph[G,N]): Seq[Set[N]] = {
    val (g,nodeMap) = DirectedGraph.fastGraph(_g)

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
      for(w <- g.children(v)){
        if(index(w) == -1) {
          strongConnect(w)
          lowLink(v) = math.min(lowLink(v),lowLink(w))
        } else if (onStack(w)) {
          lowLink(v) = math.min(lowLink(v),index(w))
        }
      }

      if(lowLink(v) == index(v)) {
        //start new scc
        var newComponent: List[Int] = Nil
        var w = 0
        do{
          w = stack.pop()
          onStack -= w
          newComponent = w :: newComponent
        } while(w != v)
        sccs = newComponent.toSet :: sccs
      }
    }

    for(v <- g.nodes){
      if(index(v) == -1) strongConnect(v)
    }

    sccs.reverse.map(_.map(nodeMap.backward))
  }
}
