package vultura.fastfactors

import scala.collection.mutable

/**
 * Belief Propagation on loopy graphs using ``FastFactor` operations.
 * User: Thomas Geier
 * Date: 6/10/13
 */
object BeliefPropagation {

  def loopyBeliefPropagation(factors: IndexedSeq[FastFactor], ring: RingZ[Double], domains: Array[Int]): Double = {
    val cg = createBetheClusterGraph(factors)
    val messages: mutable.HashMap[(Int,Int),FastFactor] = ???
    val clusterFactors: IndexedSeq[FastFactor] = ???
    def updateMessage(from: Int, to: Int): Unit = {
      //multiply all incoming messages to `from` without that from `to` with `from`'s factor
      val newFactor = FastFactor.multiplyRetain(ring)(domains)(
        factors = cg.neighbours(from)
          .filterNot(_ == to)
          .map(fromNeighbour => messages((fromNeighbour,from))) :+ clusterFactors(from),
        cg.sepsets((from,to))
      )
      //mutate message
      //TODO use sumProduct message directly and write to existing array?
      val residual = ring.maxNorm(messages((from,to)).values,newFactor.values)
      messages((from,to)) = newFactor
    }
    ???
  }


  def createBetheClusterGraph(factors: IndexedSeq[FastFactor]): ClusterGraph  = {
    //we append a cluster for each variable after the factor clusters
    val variables = factors.flatMap(_.variables).toSet.toArray
    //variables here are not shifted
    val vars2factors: Map[Int,Array[Int]] = factors.zipWithIndex
      .flatMap{case (f,fi) => f.variables.map(v => v -> fi)}
      .groupBy(_._1)
      .map{case (v,pairings) => v -> (pairings.map(_._2)(collection.breakOut): Array[Int])}

    val numVariables = variables.size
    val variableShift: Int = factors.size //shift a variable by this number to get its cluster index
    ClusterGraph(
      clusters = (factors.map(_.variables)(collection.breakOut): Array[Array[Int]]) ++ variables.map(Array[Int](_)),
      clusterFactors = factors.map(Some(_)) ++ Seq.fill(numVariables)(None),
      neighbours = (factors.map(_.variables.map(_ + variableShift))(collection.breakOut): Array[Array[Int]]) ++ variables.map(vars2factors),
      sepsets = vars2factors.flatMap{case (v,fs) => fs.map(f => (v + variableShift,f) -> Array(v)) ++ fs.map(f => (f,v + variableShift) -> Array(v))}
    )
  }

  /**
   *
   * @param clusters `clusters(i)` contains all variables in cluster `i`.
   * @param clusterFactors Contains a factor for each cluster, or `None`, if the neutral factor shall be assumed.
   * @param neighbours `neighbours(i)` contains the indices of all neighbouring clusters.
   * @param sepsets `sepsets((i,j))` contains all variables, that are shared over edge `(i,j)`, for some connected
   *               clusters `i` and `j`.
   */
  case class ClusterGraph(clusters: Array[Array[Int]],
                          clusterFactors: IndexedSeq[Option[FastFactor]],
                          neighbours: Array[Array[Int]],
                          sepsets: Map[(Int,Int),Array[Int]])
}
