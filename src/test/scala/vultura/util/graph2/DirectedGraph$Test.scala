package vultura.util.graph2

import org.specs2.mutable.Specification

class DirectedGraph$Test extends Specification {
  "constructing a graph from a map" >> {
    import TupleSeqsAreGraphs._
    DirectedGraph.materialize(Seq(1->2)).nodes === Set(1,2)
    DirectedGraph.materialize(Seq(1->2)).edges === Set((1,2))
    DirectedGraph.materialize(Seq(1->2)).parents(1) === Set()
    DirectedGraph.materialize(Seq(1->2)).parents(2) === Set(1)
  }
}
