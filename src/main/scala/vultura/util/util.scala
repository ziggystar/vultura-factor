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
  def maxByMultiple[A,B: Ordering](s: Seq[A])(fitness: (A) => B): Seq[A] = {
    val seq: Seq[(B, A)] = s.map(x => (fitness(x), x))
    val max = seq.maxBy(_._1)._1
    seq.filter(t => t._1 == max).map(_._2)
  }

  /** @return None if partition function is not positive. */
  def drawRandomlyBy[A](s: Iterable[A], random: Random)(weight: A => Double): Option[A] = {
    val partitionFunction: Double = s.map(weight).sum
    //only generate a result if partition function is positive
    Some(partitionFunction).filter(_ > 0).map{ pf =>
      assert(!pf.isInfinite, "got infinite partition function")
      val sampleWeight = random.nextDouble() * pf

      s.map(a => (a,weight(a))).scanLeft((null.asInstanceOf[A],0d)){case ((_,acc),(assignment,w)) => (assignment,acc + w)}
        .find(_._2 > sampleWeight).get._1
    }
  }

  /** @return None if partition function is not positive. */
  def drawRandomlyByIS[A](s: IndexedSeq[A], random: Random, partition: Option[Double] = None)(weight: A => Double): Option[A] = {
    val partitionFunction: Double = partition.getOrElse(s.map(weight).sum)
    //only generate a result if partition function is positive
    Some(partitionFunction).filter(_ > 0).map{ pf =>
      val sampleWeight = random.nextDouble() * pf

      s.map(a => (a,weight(a))).scanLeft((null.asInstanceOf[A],0d)){case ((_,acc),(assignment,w)) => (assignment,acc + w)}
        .find(_._2 > sampleWeight).get._1
    }
  }

  class RichRandomSeq[A](val s: IndexedSeq[A]) {
    def pickRandom(r: Random): A = s(r.nextInt(s.size))
    def pickRandomOpt(r: Random): Option[A] = if(s.isEmpty) None else Some(s(r.nextInt(s.size)))
  }

  implicit def seq2randomSeq[A](s: Iterable[A]) = new RichRandomSeq(s.toIndexedSeq)
  implicit def array2randomSeq[A](s: Array[A]) = new RichRandomSeq(s)

  //convert nested sequences to AArray which provides the crossProduct function
  type AA[A] = Array[Array[A]]
  implicit def seqseq2aa[T: ClassManifest](ss: Seq[Seq[T]]): AA[T] = ss.map(_.toArray).toArray
  implicit def seqarray2aa[T: ClassManifest](sa: Seq[Array[T]]): AA[T] = sa.toArray
  implicit def arraySeq2aa[T: ClassManifest](as: Array[Seq[T]]): AA[T] = as.map(_.toArray)

  /** Convenience method. */
  def crossProduct[T: ClassManifest](aa: AA[T]) = new DomainCPI(aa)
  /** Take a random element from each entry in aa. */
  def randomAssignment[T: ClassManifest](aa: AA[T], random: Random): Array[T] = aa.map(a => a(random.nextInt(a.size)))
  implicit def iteratorLast[A](it: Iterator[A]) = new {
    def last: A = {
      var elem = it.next()
      while(it.hasNext){elem = it.next()}
      elem
    }
  }
  def randomFlip(ordering: scala.IndexedSeq[Int], random: Random): IndexedSeq[Int] = {
    val (i1, i2) = (random.nextInt(ordering.size), random.nextInt(ordering.size))
    val builder = IndexedSeq.newBuilder[Int]
    builder.sizeHint(ordering.size)
    var idx = 0
    while (idx < ordering.size) {
      if (idx == i2)
        builder += ordering(i1)
      else if (idx == i1)
        builder += ordering(i2)
      else
        builder += ordering(idx)
      idx += 1
    }
    builder.result()
  }

  def addLog(a: Double, b: Double): Double =
    if(a.isNegInfinity)
      b
    else if(b.isNegInfinity)
      a
    else
      a + math.log1p(math.exp(b - a))
  def addLogApproximate(a: Double, b: Double): Double = if(math.abs(a - b) > 15)  a max b else addLog(a,b)

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

  def unionWithoutContradiction[A,B](ma: Map[A,B], mb: Map[A,B]): Option[Map[A,B]] = {
    val builder = Map.newBuilder[A,B]
    builder ++= ma
    mb.iterator.foreach{ elemb =>
      if(ma.get(elemb._1).exists(_ != elemb._2))
        return None
      builder += elemb
    }
    Some(builder.result())
  }
}
