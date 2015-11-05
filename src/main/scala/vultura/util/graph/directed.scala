package vultura.util.graph

import vultura.util.graph.IsDirectedGraph.DiGraphWrapper

/** Import this for graph operations. */
object directed {
  implicit class toGraphExtension[X,N](val x: X)(implicit dg: IsDirectedGraph[X,N]) {
    def diGraphView: DiGraphWrapper[X, N] = DiGraphWrapper(x)
    def diGraph: LabeledGraph[N] = LabeledGraph(x)
  }
}
