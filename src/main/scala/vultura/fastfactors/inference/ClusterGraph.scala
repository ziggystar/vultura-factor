package vultura.fastfactors.inference

import vultura.fastfactors.FastFactor

/**
  *
  * @param clusterFactors Contains a factor for each cluster, or `None`, if the neutral factor shall be assumed.
  * @param neighbours `neighbours(i)` contains the indices of all neighbouring clusters.
  * @param sepsets `sepsets((i,j))` contains all variables, that are shared over edge `(i,j)`, for some connected
  *               clusters `i` and `j`.
  */
case class ClusterGraph(clusterFactors: IndexedSeq[FastFactor],
                         neighbours: Array[Array[Int]],
                         sepsets: Map[(Int,Int),Array[Int]]) {
  //assert(sepsets.keySet == neighbours.zipWithIndex.map{case (n,i) => n.map(i -> _)}(collection.breakOut))
  def edges: Iterable[(Int,Int)] = for((sinks,srcI) <- neighbours.zipWithIndex; sinkI <- sinks) yield (srcI,sinkI)
}
