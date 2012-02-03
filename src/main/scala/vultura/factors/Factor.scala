package vultura.factors

import scalaz.Monoid

/**
 * Generic multi-variate functions.
 *
 * @author Thomas Geier
 * Date: 30.01.12
 */

trait Factor[A, B] {
  def variables(f: A): Array[Int]

  def evaluate(f: A, assignment: Array[Int]): B

  def condition(f: A, variable: Int, value: Int): A

  def marginalize[C](d: A, variables: Array[Int], domains: Array[Array[Int]])(implicit monoid: Monoid[B], newFun: Factor[C, B]): C
}

