package vultura

import scala.language.implicitConversions
import scala.util.Random
import collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom


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
  def maxByMultiple[A,B: Ordering](s: Seq[A])(fitness: (A) => B): IndexedSeq[A] = {
    val maxes = new ArrayBuffer[A]()
    val ord = implicitly[Ordering[B]]
    var max: B = fitness(s(0))

    s.foreach{ x =>
      val v: B = fitness(x)
      val cmp = ord.compare(max,v)
      if(cmp < 0){
        max = v
        maxes.clear()
        maxes += x
      } else if(cmp == 0) {
        maxes += x
      }
    }
    maxes
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

  def wheelOfFortune(xs: IndexedSeq[Double], random: Random, partition: Option[Double] = None): Int = {
    val z = partition.getOrElse(xs.sum)
    val sample = random.nextDouble() * z
    var i = 0
    var sum = xs(i)
    while(sum <= sample){
      i = i + 1
      sum = sum + xs(i)
    }
    i
  }

  class RichRandomSeq[A](val s: IndexedSeq[A]) {
    def pickRandom(r: Random): A = s(r.nextInt(s.size))
    def pickRandomOpt(r: Random): Option[A] = if(s.isEmpty) None else Some(s(r.nextInt(s.size)))
  }

  implicit def seq2randomSeq[A](s: Iterable[A]) = new RichRandomSeq(s.toIndexedSeq)
  implicit def array2randomSeq[A](s: Array[A]) = new RichRandomSeq(s)

  //convert nested sequences to AArray which provides the crossProduct function
  type AA[A] = Array[Array[A]]
  implicit def seqseq2aa[T: ClassTag](ss: Seq[Seq[T]]): AA[T] = ss.map(_.toArray).toArray
  implicit def seqarray2aa[T: ClassTag](sa: Seq[Array[T]]): AA[T] = sa.toArray
  implicit def arraySeq2aa[T: ClassTag](as: Array[Seq[T]]): AA[T] = as.map(_.toArray)

  /** Convenience method. */
  def crossProduct[T: ClassTag](aa: AA[T]) = new DomainCPI(aa)
  /** Take a random element from each entry in aa. */
  def randomAssignment[T: ClassTag](aa: AA[T], random: Random): Array[T] = aa.map(a => a(random.nextInt(a.size)))
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

  /** Create several Random objects in a deterministic way from an initial one. */
  implicit def random2RichRandom(r: Random) = new {
    def split(i: Int): Seq[Random] = {
      val seed = r.nextInt()
      (1 to i).map(n => new Random(seed + n))
    }
  }

  implicit class MyRichMap[A,B](val m: Map[A,B]) extends AnyVal {
    def toArrayMap(implicit intKeys: A <:< Int, ct: ClassTag[B]): Array[B] = {
      val maxIndex = m.keys.map(intKeys).max
      val result = new Array[B](maxIndex)
      m.foreach{case (k,v) => result(k) = v}
      result
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

  /** @return The maximum of the absolute element-wise differences between the arrays. */
  def maxDiff(as: Array[Double], bs: Array[Double]): Double = {
    var i = 0
    var max = Double.NegativeInfinity
    while(i < as.length){
      val newDelta: Double = math.abs(as(i) - bs(i))
      if(newDelta > max)
        max = newDelta
      i += 1
    }
    max
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
    ) yield successor -> node

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

  implicit class RichTraversable[A,R](val xs: TraversableLike[A,R]) extends AnyVal {
    def groupByMap[B,That,C](by: A => B, f: A => C)(implicit ev: R <:< TraversableLike[A,R], bf: CanBuildFrom[R, C, That]): collection.immutable.Map[B,That] =
      xs.groupBy(by).map{case (k,v) => k -> v.map(f)}
  }

  implicit class RichSSet[A](val sset: Set[Set[A]]) extends AnyVal {
    def isPairwiseDisjoint: Boolean = (for{
      s1 <- sset
      s2 <- sset if s1 != s2
    } yield s1 intersect s2).forall(_.isEmpty)
  }

  implicit class RichOrdering[T](val o: Ordering[T]) extends AnyVal {
    def andThen(other: Ordering[T]): Ordering[T] = new Ordering[T]{
      override def compare(x: T, y: T): Int = {
        val c1 = o.compare(x,y)
        if(c1 == 0)
          other.compare(x,y)
        else c1
      }
    }
  }
}
