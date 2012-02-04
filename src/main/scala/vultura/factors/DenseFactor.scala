package vultura.factors

import scalaz._
import Scalaz._
import vultura.util._

/**
 * Stores all function values inside an array.
 * `variables` is guaranteed to be sorted.
 *
 * @param variables
 * @param domains
 * @param data
 */
class DenseFactor[@specialized T: ClassManifest] protected[DenseFactor](val variables: Array[Int], val domains: Array[Array[Int]], val data: Array[T]) {
  val cpi = new CrossProductIndexer(domains.map(_.size))

  def evaluate(assignment: Array[Int]): T = {
    val domainIndices: Array[Int] = assignment.zip(domains).map(t => t._2.indexOf(t._1))
    val index: Int = cpi.array2Index(domainIndices)
    data(index)
  }
}


object DenseFactor {
  def fromFunction[T: ClassManifest](_vars: Seq[Int], _domains: Seq[Array[Int]], f: Array[Int] => T) = {
    require(_vars.size == _domains.size, "variable number and domain number don't match")

    val (sortedVars, sortedDomains) = _vars.zip(_domains).sortBy(_._1).unzip

    val cpi = new DomainCPI(seqarray2aa(sortedDomains))
    val table = new Array[T](cpi.size)
    var i = 0
    cpi.iterator.foreach {
      assign =>
        table(i) = f(assign)
        i += 1
    }

    new DenseFactor(sortedVars.toArray, sortedDomains.map(_.toArray).toArray, table)
  }

  implicit def tfAsFun[T: ClassManifest] = new Factor[DenseFactor[T], T, DenseFactor[T]] {
      def variables(f: DenseFactor[T]): Array[Int] = f.variables

      def domains(f: DenseFactor[T]): Array[Array[Int]] = f.domains

      def evaluate(f: DenseFactor[T], assignment: Array[Int]): T = f.evaluate(assignment)

      /** Condition via marginalization. */
      def condition(f: DenseFactor[T],
                    variables: Array[Int],
                    values: Array[Int]): DenseFactor[T] =
        this.marginalize(f,variables,values.map(Array(_)))(null)

      /** Delegate to marginalizeDense. */
      def marginalize(f: DenseFactor[T],
                      variables: Array[Int],
                      domains: Array[Array[Int]])(implicit monoid: Monoid[T]): DenseFactor[T] =
        this.marginalizeDense(f,variables,domains)
    }
}