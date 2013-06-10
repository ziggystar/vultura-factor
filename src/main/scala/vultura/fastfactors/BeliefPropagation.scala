package vultura.fastfactors

import scala.collection.mutable
import scala.util.Random

/**
 * Belief Propagation on loopy graphs using ``FastFactor` operations.
 * User: Thomas Geier
 * Date: 6/10/13
 */
object BeliefPropagation {

  def loopyBeliefPropagation(factors: IndexedSeq[FastFactor], ring: RingZ[Double], domains: Array[Int], maxiter: Int = 1000, tol: Double = 1e-5): Double = {
    val cg = createBetheClusterGraph(factors,domains)
    val messages: mutable.HashMap[(Int,Int),FastFactor] = ???

    /** @return The residual of the update. */
    def updateMessage(edge: (Int,Int)): Double = {
      val (from,to) = edge
      //multiply all incoming messages to `from` without that from `to` with `from`'s factor
      val newFactor = FastFactor.multiplyRetain(ring)(domains)(
        factors = cg.neighbours(from)
          .filterNot(_ == to)
          .map(fromNeighbour => messages((fromNeighbour,from))) :+ cg.clusterFactors(from),
        cg.sepsets(edge)
      )
      //mutate message
      //TODO use sumProduct message directly and write to existing array?
      messages(edge) = newFactor
      ring.maxNorm(messages(edge).values,newFactor.values)
    }

    val randomOrder: IndexedSeq[(Int, Int)] = Random.shuffle(messages.keys.toIndexedSeq)
    var iterations = 0
    var maxresidual = Double.PositiveInfinity
    while(iterations <= maxiter && maxresidual > tol){
      maxresidual = randomOrder.map(updateMessage).max
      iterations += 1
    }

    ???
  }


  def createBetheClusterGraph(factors: IndexedSeq[FastFactor],domains: Array[Int]): ClusterGraph  = {
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
      clusterFactors = factors ++ variables.map(v => FastFactor(Array[Int](v),???)),
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
                          clusterFactors: IndexedSeq[FastFactor],
                          neighbours: Array[Array[Int]],
                          sepsets: Map[(Int,Int),Array[Int]])
}
