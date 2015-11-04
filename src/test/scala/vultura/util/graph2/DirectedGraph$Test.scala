package vultura.util.graph2

import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment
import vultura.util.graph._
import IsDirectedGraph._
import TupleSeqsAreGraphs._

class DirectedGraph$Test extends Specification {

  trait GraphRep {
    def apply[X,N](x: X)(implicit dg: IsDirectedGraph[X,N]): DiGraphOps[N]
  }
  case object GraphView extends GraphRep {
    override def apply[X, N](x: X)(implicit dg: IsDirectedGraph[X, N]): DiGraphOps[N] = x.diGraphView
  }
  case object LGraph extends GraphRep {
    override def apply[X, N](x: X)(implicit dg: IsDirectedGraph[X, N]): DiGraphOps[N] = x.diGraph
  }

  doChecksFor(GraphView)
  doChecksFor(LGraph)

  def doChecksFor(gr: GraphRep): Fragment = {
    val chain2: DiGraphOps[Int] = gr(Seq(1 -> 2))
    val chain3: DiGraphOps[Int] = gr(Seq(1 -> 2, 2 -> 3))

    s"graph checks for $gr" >> {
      "constructing a graph from a map" >> {
        chain2.nodes === Set(1, 2)
        chain2.edges === Set((1, 2))
        chain2.parents(1) === Set()
        chain2.parents(2) === Set(1)
      }

      "basic methods on DiGraphView" >> {
        "chain 1 -> 2 -> 3" >> {
          "parents of 3" >> (chain3.parents(3) === Set(2))
          "descendants of 1" >> (chain3.descendants(1) === Set(2, 3))
          "children of 1" >> (chain3.children(1) === Set(2))
          "children of 2" >> (chain3.children(2) === Set(3))
          "ancestors of 3" >> (chain3.ancestors(3) === Set(1, 2))
          "transpose" >> (chain3.transpose must beEqualToGraph(Seq(3 -> 2, 2 -> 1)))
        }
      }

      "filtering nodes" >> {
        chain2.filterNodes(_ == 1) must beEqualToGraph(LabeledGraph.fromChildList(Seq(1),(_:Int) => Nil))
      }

      "graph equality" >> {
        "simple graph" >> {
          chain3 must beEqualToGraph(Seq(1 -> 2, 2 -> 3))
        }
      }

      "transpose" >> {
        "triangle" >> {
          gr(Seq('a -> 'b, 'b -> 'c, 'c -> 'a)).transpose must beEqualToGraph(Seq('a -> 'c, 'c -> 'b, 'b -> 'a))
        }
      }

      "strongly connected components" >> {
        "1 -> 2" >> (chain2.tarjanSCC.toSet === Set(Set(1), Set(2)))

        "1 -> 2 -> 3" >> (chain3.tarjanSCC.toSet === Set(1, 2, 3).map(Set(_)))

        "on wikipedia example" >> {
          //https://commons.wikimedia.org/wiki/File:Scc.png
          val wikiGraph = Seq(
            'a -> 'b,
            'b -> 'f, 'b -> 'c, 'b -> 'e,
            'c -> 'g, 'c -> 'd,
            'd -> 'c, 'd -> 'h,
            'e -> 'a, 'e -> 'f,
            'f -> 'g,
            'g -> 'f,
            'h -> 'd, 'h -> 'g)
          gr(wikiGraph).tarjanSCC.toSet === Set(Set('a, 'b, 'e), Set('f, 'g), Set('c, 'd, 'h))
        }

        "two pairs" >> {
          gr(Seq(
            'a -> 'b, 'b -> 'a,
            'c -> 'd, 'd -> 'c,
            'a -> 'c
          )).tarjanSCC.toSet === Set(Set('a, 'b), Set('c, 'd))
        }
      }

      "cyclicity" >> {
        "chain 3" >> (chain3.isAcyclic must beTrue)
        "loop 3" >> (gr(Seq(1 -> 2, 2 -> 3, 3 -> 1)).isAcyclic must beFalse)
      }
    }
  }

  def beEqualToGraph[Y,X,N](x: X)(implicit dgy: IsDirectedGraph[Y,N], dgx: IsDirectedGraph[X,N]): Matcher[Y] =
    beTrue.^^((_: Y).diGraphView.graphEqual(x))
}
