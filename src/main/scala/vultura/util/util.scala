package vultura

import scala.util.Random

/**
 * Utility functions.
 *
 * User: Thomas Geier
 * Date: 21.03.11
 */

package object util {
  /**
   * @return Those elements of s where fitness evaluates to the highest value.
   */
  def maxByMultiple[A](s: Seq[A])(fitness: (A) => Int): Seq[A] = {
    val seq = s.map(x => (fitness(x), x))
    val max = seq.max(Ordering.by {
      t: (Int, A) => t._1
    })._1
    seq.filter(t => t._1 == max).map(_._2)
  }

  class RichRandomSeq[A](val s: IndexedSeq[A]) {
    def pickRandom(r: Random): A = s(r.nextInt(s.size))
  }

  implicit def seq2randomSeq[A](s: Iterable[A]) = new RichRandomSeq(s.toIndexedSeq)
  implicit def array2randomSeq[A](s: Array[A]) = new RichRandomSeq(s)

  //convert nested sequences to AArray which provides the allCombinations function
  type AA[A] = Array[Array[A]]
  implicit def seqseq2aa[T: ClassManifest](ss: Seq[Seq[T]]): AA[T] = ss.map(_.toArray).toArray.asInstanceOf[AA[T]]
  implicit def seqarray2aa[T: ClassManifest](sa: Seq[Array[T]]): AA[T] = sa.toArray.asInstanceOf[AA[T]]
  implicit def aa2aa[T: ClassManifest](aa: Array[Array[T]]): AA[T] = aa.asInstanceOf[AA[T]]

  implicit def statisticsPimper[A: Numeric](xs: Iterable[A]) = new {
      def mean: Double = implicitly[Numeric[A]].toDouble(xs.sum) / xs.size

      def variance: Double = {
        if (xs.size == 1) return 0
        val mean_s: Double = mean
        xs.map(implicitly[Numeric[A]].toDouble(_) - mean_s).map(x => x * x).sum / (xs.size - 1)
      }
    }

  /** Create several Random objects in a deterministic way from an initial one. */
  implicit def random2RichRandom(r: Random) = new {
    def split(i: Int): Seq[Random] = {
      val seed = r.nextInt()
      (1 to i).map(n => new Random(seed + n))
    }
  }

  /**
   * Given a multi-map that is interpreted as a directed graph with keys mapping to all their successors,
   * fill in all transitive edges.
   */
  def transitiveClosure[A](directedGraph: Map[A, Set[A]]): Map[A, Set[A]] = {
    //iterate over the growing multi-map and an indicator whether something has changed
    val transitiveWalker = Iterator.iterate(
      (directedGraph, true)
    ) {
      case (relations, _) =>
        val elementsZipped: Set[((A, Set[A]), Boolean)] = for (s <- relations.keySet) yield {
          val successors: Set[A] = relations(s)

          val succSuccessors = successors.flatMap(relations.get(_)).flatMap(x => x)

          val newSupers = successors ++ succSuccessors
          (s -> newSupers, newSupers.size > successors.size)
        }

        val (mapEntries, flags) = elementsZipped.unzip
        (mapEntries.toMap, flags.exists(t => t))
    }

    transitiveWalker.dropWhile(_._2).next()._1.withDefaultValue(Set[A]())
  }

  /**
   * Regarding `directedGraph` as a directed graph with edges going from the key to all of the members of
   * its value set, then we simply reverse all edges and return the result in the same format.
   *
   * Important: the map should be defined for all nodes, so directedGraph.keySet contains all nodes.
   */
  def reverseMultiMap[A](directedGraph: Map[A, Set[A]]): Map[A, Set[A]] = {
    val nodes = directedGraph.keySet

    //this are all the edges of the reversed graph
    val inverted = for (
      node <- nodes;
      successor <- directedGraph(node)
    ) yield (successor -> node)

    inverted
      .groupBy(_._1) //Map[A,Set[(A,A)]]
      .mapValues(_.map(_._2))
  }
}
