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
