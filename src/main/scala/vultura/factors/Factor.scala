package vultura.factors

import scalaz._
import Scalaz._
import vultura.util._

/**
 * Generic multi-variate functions type class.
 *
 * @tparam A This type functions as a factor.
 * @tparam B This is the result type of the factor. This means the return type of `evaluate`.
 *  implementations, e.g. when counting SAT problem solutions.
 * @author Thomas Geier
 * @since 30.01.12
 */

trait Factor[A,B] {
  def variables(f: A): Array[Int]

  def domains(f: A): Array[Array[Int]]

  def evaluate(f: A, assignment: Array[Int]): B

  /**This is provided to retain the type of the factor after conditioning. E.g. SAT clauses can be conditioned
   * but not marginalized without loosing their type.
   */
  def condition(f: A, variables: Array[Int], value: Array[Int]): A
}

/**A factor implementation can provide an instance of this type-class if it supports marginalization to a certain
 * target type 'M'.
 * @tparam T The type of the factor.
 * @tparam R Factor evaluates to this result.
 * @tparam M Supports marginalization to this type.
 */
trait TransMarginalize[T,R,M]{
  /**
   * Generalization of marginalization and conditioning. This method sums over all given domain values
   * for the specified variables. Specifying only one domain value for a variable is equivalent to conditioning
   * on this value.
   * Note: The function must not rely on `monoid` being non-null if no domain contains more than one entry.
   *
   * @param f Object to apply to.
   * @param variables These variables will be eliminated from the function.
   * @param domains Zips with `variables`. Contains at least one domain value for each variable in `variables`. If multiple values are given for
   *  a certain variable, the variable is eliminated by summing over the given values.
   * @param monoid Use this for summing the result values of type `T`.
   * @return A `Factor` that does not depend on any variable in `variables`.
   */
  def marginalize(f: T, variables: Array[Int], domains: Array[Array[Int]])(implicit evF: Factor[T,R], monoid: Monoid[R]): M
}