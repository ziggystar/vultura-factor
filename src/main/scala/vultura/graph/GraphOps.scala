package vultura.graph

import collection.mutable.{Queue, HashSet}
import nl.uu.cs.treewidth.input.GraphInput.InputData
import nl.uu.cs.treewidth.ngraph._

import scalaz._
import Scalaz._
import nl.uu.cs.treewidth.algorithm._
import collection.Iterable
import collection.immutable.{Set, Map}

/**
 * Type class for undirected graphs whose nodes are of type A.
 *
 * User: Thomas Geier
 * Date: 23.09.11
 */

object GraphOps {
  def nodes[G,A](g: G)(implicit c: Graph[G,A]): Set[A] = c.nodes(g)
  def adjacent[G,A](g: G, n1: A, n2: A)(implicit c: Graph[G,A]): Boolean = c.adjacent(g,n1,n2)
  def edges[G,A](g: G)(implicit c: Graph[G,A]): Set[(A,A)] = c.edges(g)
  def neighbours[G,A](g: G, v: A)(implicit c: Graph[G,A]): Set[A] = c.neighbours(g,v)

  def hasSelfCycle[G,A](g: G)(implicit c: Graph[G,A]): Boolean = edges(g).exists(e => e._1 == e._2)

  def componentOf[G,A](g: G, n: A)(implicit c: Graph[G,A]): Set[A] = componentOfIsTree(g,n)._1

  def asTreeWithRoot[G,A](g: G, r: A)(implicit c: Graph[G,A]): Option[Tree[A]] = {
    def buildTreeRec(root: A, parent: Option[A], visited: Set[A]): Tree[Option[A]] = {
      if(visited(root))
        leaf(None)  //if root was already visited
      else {
        val children = parent.map(neighbours(g,root) - _).getOrElse(neighbours(g,root))
        if(children.isEmpty)
          leaf(Some(root))
        else
          node(Some(root), children.toStream.map(buildTreeRec(_,Some(root),visited + root)))
      }
    }

    buildTreeRec(r, None, Set.empty[A]).sequence
  }

  def componentOfIsTree[G,A](g: G, n: A)(implicit c: Graph[G,A]): (Set[A],Boolean) = {
    assert(nodes(g).contains(n), "graph must contain the node for extracting a component")

    val visited = new HashSet[A]
    val fringe = new Queue[(A,A)]
    var isTree = true

    def isFreshInvalidateTree(n: A) = {
      val isFresh = !visited.contains(n)
      if(!isFresh) {isTree = false}
      isFresh
    }

    //next is always pair of predecessor and successor node of a followed edge
    var (predNext,next) = (n, n)
    //only false for the root
    var predValid = false

    var finish = false
    while( !finish ) {
      //mark node
      visited += next

      //compute the successors of next._2
      val successors: List[(A, A)] = for(
        neighbour <- neighbours(g,next).toList;
        successor = neighbour if(!predValid || neighbour != predNext);
        freshSuccessor = successor if (isFreshInvalidateTree(successor))
      ) yield next -> freshSuccessor

      successors match {
        case Nil if (fringe.isEmpty) => {finish = true}
        case Nil => {
          val t = fringe.dequeue()
          predNext = t._1
          next = t._2
        }
        case head :: rest => {
          predNext = head._1
          next = head._2
          fringe.enqueue(rest:_*)
        }
      }
      predValid = true
    }
    (visited.toSet, isTree)
  }

  def componentsTreeProperty[G,A](g: G)(implicit c: Graph[G,A]): Set[(Set[A],Boolean)] = {
    val cStream: Stream[(Set[A], List[(Set[A],Boolean)])] = Stream.iterate((nodes(g),List.empty[(Set[A],Boolean)])){case (remaining,components) =>
      val cTreeProperty@(newComponent,_) = if(remaining.isEmpty) (Set.empty[A],true) else componentOfIsTree(g,remaining.head)
      (remaining -- newComponent, cTreeProperty :: components)
    }
    cStream.dropWhile(!_._1.isEmpty).head._2.toSet
  }

  def components[G,A](g: G)(implicit c: Graph[G,A]): Set[Set[A]] = componentsTreeProperty(g).map(_._1)

  def containsCycle[G,A](g: G)(implicit c: Graph[G,A]): Boolean = componentsTreeProperty(g).exists(!_._2)

  class VertexWrapper[A](val data: A, id: Int, name: String) extends InputData(id, name)

  def asNGraph[G,A](g: G, stripSelfLoops: Boolean = false)(implicit c: Graph[G,A]): NGraph[VertexWrapper[A]] = {
    val result = new ListGraph[VertexWrapper[A]]

    var instanceCounter = 0
    def wrapVertex(v: A): VertexWrapper[A] = {
      val result = new VertexWrapper(v,instanceCounter,v.toString)
      instanceCounter += 1
      result
    }

    val listVertices: Map[A, ListVertex[VertexWrapper[A]]] = nodes(g).map(a => a -> new ListVertex(wrapVertex(a))).toMap

    //add nodes
    nodes(g).foreach{a => result.addVertex(listVertices.apply(a))}

    val addEdges = (_: Iterable[(A,A)]).foreach{case (a1,a2) => result.addEdge(listVertices(a1),listVertices(a2))}

    if(stripSelfLoops)
      addEdges(edges(g).filter(t => !stripSelfLoops || t._1 != t._2))
    else
      addEdges(edges(g))

    result
  }

  def treeWidth[G,A](g: G)(implicit c: Graph[G,A]): Int = {
    val algorithm = new TreewidthDP[VertexWrapper[A]]
    algorithm.setInput(asNGraph(g,true))
    algorithm.run()
    algorithm.getTreewidth
  }

  def treeDecomposition[G,A](g: G)(implicit c: Graph[G,A]): Set[Tree[Set[A]]] = {
    require(edges(g).forall(e => e._1 != e._2), "no self loops allowed for tree decomposition")

    val nGraph = asNGraph(g)

    val algorithm = new AllStartLexBFS[VertexWrapper[A]]
    val conversion = new PermutationToTreeDecomposition(algorithm)

    conversion.setInput(nGraph)
    conversion.run()

    import collection.JavaConversions._

    /** Takes a NTDBag node that is used in the tree
     * decomposition as vertex type and returns the set of nodes it contains. */
    def unwrapBag(bag: NVertex[NTDBag[VertexWrapper[A]]]): Set[A] = bag.data.vertices.map(_.data.data).toSet

    val nTree: NGraph[NTDBag[VertexWrapper[A]]] = conversion.getDecomposition
    val resultVertices = nTree.getVertices.map(unwrapBag)
    val resultEdges: Iterable[(Set[A], Set[A])] = nTree.edges().map(e => unwrapBag(e.a) -> unwrapBag(e.b))

    //first create a map containing only the nodes and then add the edges.
    //This ensures that unconnected nodes are present.
    //the ListGraph returned by getDecomposition is in a very inefficient format (just a list of edges)
    val mapGraph: Map[Set[A], Set[Set[A]]] = resultVertices.map(_ -> Set.empty[Set[A]]).toMap ++
      resultEdges.flatMap(e => Seq((e._1,e._2),(e._2,e._1))).groupBy(_._1).mapValues(_.map(_._2).toSet)

    import AdjacencyMapGraph._
    val treeComponents: Set[(Set[Set[A]], Boolean)] = componentsTreeProperty(mapGraph)
    assert(treeComponents.forall(_._2), "oops, tree decomposition is not a tree?")

    treeComponents.map(_._1).map(tc => asTreeWithRoot(mapGraph, tc.head).get)
  }
}