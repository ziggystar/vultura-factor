package vultura.util

import scalaz.Monoid
import collection.IndexedSeq

/**
 * Type class that says that provides a measure for values of type `A`.
 *
 * Laws:
 * 1. a + b > 0 <=> a > 0 v b > 0
 * 2. w(a + b) == w(a) + a(b)
 * 3. a > 0 => w(a) > 0
 */
trait Measure[@specialized(Double) A] {
  def normalize(seq: Array[A]): Array[Double]
  def normalizedWeight(partition: A): A => Double
  /** @return a non-negative real */
  def weight(a: A): Double
  def sum: Monoid[A]
  /** @return true if this is a value from which one can sample. */
  def isPositive(value: A): Boolean
}

object Measure {
  val measureDouble: Measure[Double] = new Measure[Double] {

    def normalize(seq: Array[Double]): Array[Double] = {
      val p = seq.sum
      seq.map(_ / p)
    }

    def isPositive(value: Double): Boolean = value > 0
    def normalizedWeight(partition: Double): (Double) => Double = (x: Double) => x / partition
    def weight(a: Double): Double = a
    def sum: Monoid[Double] = new Monoid[Double]{
      def append(s1: Double, s2: => Double): Double = s1 + s2
      val zero: Double = 0d
    }
  }
  val measureInt: Measure[Int] = new Measure[Int] {

    def normalize(seq: Array[Int]): Array[Double] = sys.error("this will not do")

    def isPositive(value: Int): Boolean = value > 0

    def normalizedWeight(partition: Int): (Int) => Double = (x: Int) => x / partition.toDouble
    def weight(a: Int): Double = a

    def sum: Monoid[Int] = new Monoid[Int]{
      def append(s1: Int, s2: => Int): Int = 1 + s2
      val zero: Int = 0
    }
  }

  val measureBigInt: Measure[BigInt] = new Measure[BigInt] {

    def normalize(seq: Array[BigInt]): Array[Double] = sys.error("this will not do!")

    def isPositive(value: BigInt): Boolean = value > 0

    def normalizedWeight(partition: BigInt): (BigInt) => Double = (x: BigInt) => x.toDouble / partition.toDouble
    def weight(a: BigInt): Double = a.toDouble

    def sum: Monoid[BigInt] = new Monoid[BigInt]{
      def append(s1: BigInt, s2: => BigInt): BigInt = 1 + s2
      val zero: BigInt = 0
    }
  }
}
