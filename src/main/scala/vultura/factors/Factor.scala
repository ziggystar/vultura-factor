package vultura.factors

import scalaz._
import Scalaz._
import vultura.util._

/**
 * Generic multi-variate functions type class.
 *
 * @tparam A This type functions as a factor.
 * @tparam B This is the result type of the factor. This means the return type of `evaluate`.
 * @tparam C This is the type that is returned in case of marginalization. This might be different from `A` for some
 *  implementations, e.g. when counting SAT problem solutions.
 * @author Thomas Geier
 * @since 30.01.12
 */

trait Factor[A, B, C] {
  def variables(f: A): Array[Int]

  def domains(f: A): Array[Array[Int]]

  def evaluate(f: A, assignment: Array[Int]): B

  /**This is provided to retain the type of the factor after conditioning. E.g. SAT clauses can be conditioned
   * but not marginalized without loosing their type.
   */
  def condition(f: A, variables: Array[Int], value: Array[Int]): A

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
  def marginalize(f: A, variables: Array[Int], domains: Array[Array[Int]])(implicit monoid: Monoid[B]): C

  def marginalizeDense(f: A, _vars: Array[Int], _doms: Array[Array[Int]])(implicit monoid: Monoid[B], manifestB: ClassManifest[B]): DenseFactor[B] = {
      val (vars, doms) = _vars.zip(_doms).sortBy(_._1).unzip

      val MASK = 0x80000000

      val (variables,domains) = (this.variables(f),this.domains(f))

      val (remainingVars, remainingDomains) = variables.zip(domains).filterNot(t => vars.contains(t._1)).unzip

      //this tells us where to look for a value when constructing an assignment to this Fun
      //if high bit is unset remainingVars and else to the summed out vars
      //this is not in imperative style for optimization purposes but for readability:/
      val lookUp: Array[Int] = {
        var intoVars = 0
        var intoRemaining = 0
        var intoVariables = 0
        val result = new Array[Int](variables.size)
        while (intoVariables < variables.size) {
          if (intoVars < vars.size && variables(intoVariables) == vars(intoVars)) {
            result(intoVariables) = intoVars | MASK
            intoVars += 1
          } else {
            assert(variables(intoVariables) == remainingVars(intoRemaining))
            result(intoVariables) = intoRemaining
            intoRemaining += 1
          }
          intoVariables += 1
        }

        result
      }

      val _cpi = new DomainCPI(doms)
      val reusedAssignment = new Array[Int](variables.size)

      //fix the assignment to the remaining variables and sum over everything in vars/doms
      //the argument to sumOut goes to the remaining variables
      val sumOut: Array[Int] => B = {
        assignment =>
          def reconstructAssignment(remainAss: Array[Int], newAss: Array[Int]): Array[Int] = {
            var i = 0
            while (i < reusedAssignment.size) {
              val index = lookUp(i)
              val value = if ((index & MASK) != 0) newAss(index ^ MASK) else remainAss(index)
              reusedAssignment(i) = value
              i += 1
            }
            reusedAssignment
          }

          _cpi.iterator.map {
            newAssign =>
              evaluate(f,reconstructAssignment(assignment, newAssign))
          }.reduce(_ |+| _)
      }

      DenseFactor.fromFunction(remainingVars, remainingDomains, sumOut)
    }
}