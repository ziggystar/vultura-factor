package vultura.factors

/**
 * A class that implements this trait is a factor.
 * @author Thomas Geier
 * @since 05.02.12
 */

trait SelfFactor[R]{
  def variables: Array[Int]
  def domains: Array[Array[Int]]
  def evaluate(assignment: Array[Int]): R
  def condition(variables: Array[Int], values: Array[Int]): SelfFactor[R]
}

object SelfFactor {
  implicit def sf2f[R,SF <: SelfFactor[R]]: Factor[SF,R] = new Factor[SF,R] {
    def variables(f: SF): Array[Int] = f.variables
    def domains(f: SF): Array[Array[Int]] = f.domains
    def evaluate(f: SF, assignment: Array[Int]): R = f.evaluate(assignment)
    def condition(f: SF, variables: Array[Int], value: Array[Int]): SF = f.condition(variables,value)
  }
}