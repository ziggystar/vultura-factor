package vultura.util

import scalaz._
import Scalaz._


/**
 * Type class which provides distributing addition and multiplication for values of `A`.
 */

trait RingWithZero[A] {
  def addition: AbelianGroup[A]
  def multiplication: Monoid[A]
}

object LogMeasure extends Measure[Double]{
  def normalize(seq: Array[Double]): Array[Double] = {
      //first shift the values for the maximum to be 0
      val shift = seq.max
      val shifted = seq.map(_ - shift)
      val z = shifted.view.map(math.exp).sum
      shifted.map(_ - math.log(z))
  }
  def normalizedWeight(partition: Double): (Double) => Double = (x: Double) => math.exp(x - partition)
  def sum: Monoid[Double] = RingWithZero.logSumProd.addition
  def weight(a: Double): Double = math.exp(a)
  def isPositive(value: Double): Boolean = !value.isNegInfinity
}

object RingWithZero{
  def sumProduct = new RingWithZero[Double] {
    def addition = new AbelianGroup[Double] {
      def inverse(a: Double) = -a
      def append(s1: Double, s2: => Double) = s1 + s2
      val zero = 0d
    }

    def multiplication = new Monoid[Double] {
      def append(s1: Double, s2: => Double) = s1 * s2
      val zero = 1d
    }
  }

  def logSumProd = new RingWithZero[Double] {
    def addition: AbelianGroup[Double] = new AbelianGroup[Double] {
      def inverse(a: Double): Double = sys.error("inverse not available in log domain")
      def append(s1: Double, s2: => Double): Double = vultura.util.addLogApproximate(s1,s2)
      val zero: Double = Double.NegativeInfinity
    }

    def multiplication: Monoid[Double] = new Monoid[Double]{
      def append(s1: Double, s2: => Double): Double = s1 + s2
      val zero: Double = 0d
    }
  }
}

trait AbelianGroup[A] extends Monoid[A]{
  def inverse(a: A): A
}

object AbelianGroup{
  implicit def numericAG[A: Numeric]: AbelianGroup[A] = new AbelianGroup[A]{
    def inverse(a: A): A = implicitly[Numeric[A]].negate(a)
    def append(s1: A, s2: => A): A =  implicitly[Numeric[A]].mkNumericOps(s1) + s2
    val zero: A = implicitly[Numeric[A]].zero
  }
}
