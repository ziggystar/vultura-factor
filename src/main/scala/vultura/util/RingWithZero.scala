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
