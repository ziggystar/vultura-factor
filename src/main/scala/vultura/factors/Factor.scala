package vultura.factors

import scalaz._
import Scalaz._
import collection.mutable.WrappedArray
import util.Random
import vultura.util.{Measure, DomainCPI}

/**
 * Generic multi-variate functions type class.
 *
 * @tparam A This type functions as a factor.
 * @tparam B This is the result type of the factor. This means the return type of `evaluate`.
 *  implementations, e.g. when counting SAT problem solutions.
 * @author Thomas Geier
 * @since 30.01.12
 */

sealed trait Factor[A,B] {
  def variables(f: A): Array[Int]

  def domains(f: A): Array[Array[Int]]

  def evaluate(f: A, assignment: Array[Int]): B

  /**This is provided to retain the type of the factor after conditioning. E.g. SAT clauses can be conditioned
   * but not marginalized without losing their type.
   */
  def condition(f: A, variables: Array[Int], values: Array[Int]): A

  def iterator(f: A): Iterator[(Array[Int], B)] = new DomainCPI(domains(f)).iterator.map(a => a -> evaluate(f, a))

  def partition(f: A, sumMonoid: Monoid[B])(implicit cm: ClassManifest[B]): B =
    vultura.util.crossProduct(this.domains(f)).iterator.map(this.evaluate(f,_)).reduce(sumMonoid.append(_,_))

  /**uses two traversals of the domain of the factor to generate an exact sample. */
  def sample(problem: A, random: Random)(implicit m: Measure[B]): Array[Int] = vultura.util.drawRandomlyBy(
    new DomainCPI(domains(problem)).toIterable, random)(a => m.weight(this.evaluate(problem,a)))
}

trait DenseFactor[A,B] extends Factor[A,B]
trait SparseFactor[A,B] extends Factor[A,B] {
  def evaluate(f: A, assignment: Array[Int]): B = points(f).withDefaultValue(defaultValue(f)).apply(wrapIntArray(assignment))
  def defaultValue(f: A): B
  def points(f: A): Map[WrappedArray[Int],B]

  /**
   * samples from this sparse factor. If the weight of `defaultValue` is non-zero, then rejection sampling is used
   * if, if the sample would be drawn from the default assignments. This should be efficient if the factor is indeed
   * sparse.
   */
  override def sample(f: A, random: Random)(implicit m: Measure[B]): Array[Int] = {
    def sparseWeightIterator: Iterator[(Array[Int],Double)] =
      this.points(f).iterator.map(argVal => ((_:WrappedArray[Int]).toArray) <-: argVal :-> (m.weight(_)))

    val totalSize: BigInt = this.domains(f).map(d => BigInt(d.size)).product

    val defaultWeight = (totalSize - this.points(f).size).toDouble * m.weight(this.defaultValue(f))

    val partitionFunction: Double = sparseWeightIterator.map(_._2).sum + defaultWeight

    val sampleWeight = random.nextDouble() * partitionFunction

    //we assume that the default weight comes first
    if(sampleWeight < defaultWeight) {
      //do rejection sampling to find an assignment which is not in points
      Iterator
        .continually(vultura.util.randomAssignment(this.domains(f), random))
        .filterNot(ass => points(f).contains(wrapIntArray(ass)))
        .next()
    } else {
      val correctedWeight = sampleWeight - defaultWeight
      sparseWeightIterator.scanLeft((null: Array[Int],0d)){case ((_,acc),(assignment,weight)) => (assignment,acc + weight)}
        .find(_._2 > correctedWeight).get._1
    }
  }
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