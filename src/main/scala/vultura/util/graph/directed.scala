package vultura.util.graph

/** This is the main import object for directed graphs. */
object directed {
  implicit class toGraphExtension[X,N](val x: X)(implicit dg: IsDirectedGraph[X,N]) {
    def diGraphView: DiGraphInstOps[N] = ops.WithGraphOps(x)
    def diGraph: LabeledGraph[N] = LabeledGraph(x)
  }

  /** Import this for operations directly on types that have a [[vultura.util.graph.IsDirectedGraph]] instance. */
  object ops {
    implicit class WithGraphOps[X,N](val x: X)(implicit dg: IsDirectedGraph[X,N]) extends DiGraphInstOps[N]{
      override type G = X
      override val typeClass: IsDirectedGraph[G, N] = dg
      override val instance: G = x
    }
  }

  object instances {
    /** Import for type-class instances for maps. */
    object TupleSeqs {
      implicit def tupleSeqGraphInstance[N]: IsDirectedGraph[Iterable[(N,N)],N] = new IsDirectedGraph[Iterable[(N,N)],N] {
        override def nodes(x: Iterable[(N,N)]): Set[N] = (x.map(_._1)(collection.breakOut): Set[N]) ++ x.map(_._2)
        override def edges(x: Iterable[(N,N)]): Set[(N, N)] = x.toSet
      }
    }
  }
}
