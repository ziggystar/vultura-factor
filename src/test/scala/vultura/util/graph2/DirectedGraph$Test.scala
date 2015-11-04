package vultura.util.graph2

import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import IsDirectedGraph._
import TupleSeqsAreGraphs._

class DirectedGraph$Test extends Specification {
  val chain3: DiGraphOps[Int] = Seq(1 -> 2, 2 -> 3).diGraphView

  "constructing a graph from a map" >> {
    Seq(1->2).diGraphView.nodes === Set(1,2)
    Seq(1->2).diGraphView.edges === Set((1,2))
    Seq(1->2).diGraphView.parents(1) === Set()
    Seq(1->2).diGraphView.parents(2) === Set(1)
  }

  "basic methods on DiGraphView" >> {
    "chain 1 -> 2 -> 3" >> {
      chain3.parents(3) === Set(2)
      chain3.ancestors(3) === Set(1,2)
      chain3.children(1) === Set(2)
      chain3.descendants(1) === Set(2,3)
      chain3.transpose must beEqualToGraph(Seq(3 -> 2, 2 -> 1))
    }
  }

  "filtering nodes" >> {
    Seq('a -> 'b).diGraphView.filterNodes(_ == 'a).nodes === Set('a)
    Seq('a -> 'b).diGraphView.filterNodes(_ == 'a).edges === Set()
  }

  "graph equality" >> {
    "simple graph" >> {
      Seq('a -> 'b, 'b -> 'c) must beEqualToGraph(Seq('b -> 'c, 'a -> 'b))
    }
  }

  "transpose" >> {
    "triangle" >> {
      Seq('a -> 'b, 'b -> 'c, 'c -> 'a).diGraphView.transpose must beEqualToGraph(Seq('a -> 'c, 'c -> 'b, 'b -> 'a))
    }
  }

  "strongly connected components" >> {
    "single edge example" >> (Seq('a -> 'b).diGraphView.tarjanSCC.toSet === Set(Set('a),Set('b)))

    "path 3" >> (Seq('a -> 'b, 'b -> 'c).diGraphView.tarjanSCC.toSet === Set('a,'b,'c).map(Set(_)))

    "on wikipedia example" >> {
      //https://commons.wikimedia.org/wiki/File:Scc.png
      val wikiGraph = Seq(
        'a -> 'b,
        'b -> 'f, 'b ->'c, 'b -> 'e,
        'c -> 'g, 'c -> 'd,
        'd -> 'c, 'd -> 'h,
        'e -> 'a, 'e -> 'f,
        'f -> 'g,
        'g -> 'f,
        'h -> 'd, 'h -> 'g)
      wikiGraph.diGraphView.tarjanSCC.toSet === Set(Set('a, 'b, 'e), Set('f, 'g), Set('c, 'd, 'h))
    }

    "two pairs" >> {
      Seq(
        'a -> 'b, 'b -> 'a,
        'c -> 'd, 'd -> 'c,
        'a -> 'c
      ).diGraphView.tarjanSCC.toSet === Set(Set('a,'b),Set('c,'d))
    }
  }

  def beEqualToGraph[Y,X,N](x: X)(implicit dgy: IsDirectedGraph[Y,N], dgx: IsDirectedGraph[X,N]): Matcher[Y] =
    beTrue.^^((_: Y).diGraphView.graphEqual(x))
}
