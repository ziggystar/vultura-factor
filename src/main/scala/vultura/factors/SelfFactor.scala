package vultura.factors

import scalaz.Monoid

/**
 * @author Thomas Geier
 * @since 05.02.12
 */

trait SelfFactor[B,C]{
  def variables: Array[Int]
  def domains: Array[Array[Int]]
  def evaluate(assignment: Array[Int]): B
  def condition(variables: Array[Int], value: Array[Int]): SelfFactor[B,C]
  def marginalize(variables: Array[Int], domains: Array[Array[Int]])(implicit monoid: Monoid[B]): C
}

object SelfFactor {
  implicit def sf2f[B,C] = new Factor[SelfFactor[B,C],B,C] {
    def variables(f: SelfFactor[B,C]): Array[Int] = f.variables
    def domains(f: SelfFactor[B,C]): Array[Array[Int]] = f.domains
    def evaluate(f: SelfFactor[B,C], assignment: Array[Int]): B = f.evaluate(assignment)
    def condition(f: SelfFactor[B,C], variables: Array[Int], value: Array[Int]): SelfFactor[B,C] = f.condition(variables,value)
    def marginalize(f: SelfFactor[B,C], variables: Array[Int], domains: Array[Array[Int]])(implicit monoid: Monoid[B]): C =
      f.marginalize(variables,domains)(monoid)
  }
}