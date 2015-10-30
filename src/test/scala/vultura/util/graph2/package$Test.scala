package vultura.util.graph2

import org.specs2.mutable.Specification
import TupleSeqsAreGraphs._

class package$Test extends Specification {

  "graph equality" >> {
    "simple graph" >> {
      isSameGraph(Seq('a -> 'b, 'b -> 'c), Seq('b -> 'c, 'a -> 'b)) must beTrue
    }
  }

  "transpose" >> {
    "triangle" >> {
      isSameGraph(transpose(Seq('a -> 'b, 'b -> 'c, 'c -> 'a)), Seq('a -> 'c, 'c -> 'b, 'b -> 'a)) must beTrue
    }
  }

  "dfsPostOrdering" >> {
    "single edge" >> (dfsPostOrderingForest(Seq('a -> 'b)) === List('b,'a))
    "path 3" >> (dfsPostOrderingForest(Seq('a -> 'b, 'b -> 'c)) === List('c,'b,'a))
    "diamond" >> {
      val order = dfsPostOrderingForest(Seq('a -> 'b, 'a -> 'c, 'b -> 'd, 'c -> 'd))
      order === List('d,'c,'b,'a) or order === List('d,'b,'c,'a)
    }
  }

  "strongly connected components" >> {
    "single edge example" >> (stronglyConnectedComponents(Seq('a -> 'b)) === Set(Set('a),Set('b)))

    "path 3" >> (stronglyConnectedComponents(Seq('a -> 'b, 'b -> 'c)) === Set('a,'b,'c).map(Set(_)))

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
      stronglyConnectedComponents(wikiGraph) === Set(Set('a, 'b, 'e), Set('f, 'g), Set('c, 'd, 'h))
    }

    "two pairs" >> {
      stronglyConnectedComponents(Seq(
        'a -> 'b, 'b -> 'a,
        'c -> 'd, 'd -> 'c,
        'a -> 'c
      )) === Set(Set('a,'b),Set('c,'d))
    }
  }

  "tarjan strongly connected components" >> {
    "single edge example" >> (tarjanSCC(Seq('a -> 'b)).toSet === Set(Set('a),Set('b)))

    "path 3" >> (tarjanSCC(Seq('a -> 'b, 'b -> 'c)).toSet === Set('a,'b,'c).map(Set(_)))

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
      tarjanSCC(wikiGraph).toSet === Set(Set('a, 'b, 'e), Set('f, 'g), Set('c, 'd, 'h))
    }

    "two pairs" >> {
      tarjanSCC(Seq(
        'a -> 'b, 'b -> 'a,
        'c -> 'd, 'd -> 'c,
        'a -> 'c
      )).toSet === Set(Set('a,'b),Set('c,'d))
    }

    "on unconnected graph size 1" >> { tarjanSCC(ChildList(Set(0), Map())) === List(Set(0))}
    "on unconnected graph size 2" >> {
      val scc = tarjanSCC(ChildList(Set(0,1), Map()))
       (scc === List(Set(0), Set(1))) or (scc === List(Set(1),Set(0)))
    }
  }
}
