package vultura.util.graph

import org.specs2._
import org.specs2.specification.Fragments

class EdgeBiMapDiGraphTest extends Specification {
  val simpleGraph = EdgeMapDiGraph(Set(1,2,3,4,5,6),Map(1 -> Set(2,3),2 -> Set(3,4),6 -> Set(5)))

  def is: Fragments =
  "EdgeMapDiGraph" ^
    "edges" ! (simpleGraph.edges == Set(1 -> 2, 1 -> 3, 2 -> 3, 2 -> 4, 6 -> 5)) ^
  "EdgeMapBiDiGraph" ^
    "conversion from EdgeMapDiGraph" !
      (simpleGraph.toBiMap.predecessors === Map(2 -> Set(1),3 -> Set(1),3 -> Set(1,2), 4 -> Set(2), 5 -> Set(6))) ^
    "find component of 3" ! (simpleGraph.toBiMap.undirectedComponent(3) === Set(1,2,3,4)) ^
    "subgraph" ! (simpleGraph.subgraph(Set(1,2,4).contains) === EdgeMapDiGraph(Set(1,2,4),Map(1 -> Set(2),2 -> Set(4)))) ^
    "find components of simepleGraph" ! (simpleGraph.toBiMap.components === Set(Set(5,6),Set(1,2,3,4))) ^
    "shortest path" ! (simpleGraph.shortestPath(1,4) === Some(List(1,2,4))) ^
    "shortest path (no path)" ! (simpleGraph.shortestPath(1,6) === None) ^
    "find no cycle in simpleGraph" ! (simpleGraph.findCycle === None)
}
