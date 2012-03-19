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
  def weight(a: Double*): Double = a.map(math.exp).sum
}

object RingWithZero{
  def sumProduct[A: Numeric] = new RingWithZero[A] {
    val numeric = implicitly[Numeric[A]]
    def addition = new AbelianGroup[A] {
      def inverse(a: A) = numeric.negate(a)
      def append(s1: A, s2: => A) = Seq(s1,s2).sum
      val zero = numeric.zero
    }

    def multiplication = new Monoid[A] {
      def append(s1: A, s2: => A) = Seq(s1,s2).product
      val zero = numeric.one
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
      val zero: Double = 1d
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
