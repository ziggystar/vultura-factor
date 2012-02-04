package vultura

import scalaz.Monoid

/**
 * @author Thomas Geier
 * @since 03.02.12
 */

package object factors {
  def variables[A](a: A)(implicit f: Factor[A,_,_]): Array[Int] = f.variables(a)

  def domains[A](a: A)(implicit f: Factor[A,_,_]): Array[Array[Int]] = f.domains(a)

  def evaluate[A,B](a: A, assignment: Array[Int])(implicit f: Factor[A,B,_]): B = f.evaluate(a,assignment)

  def condition[A,B](a: A, variables: Array[Int], values: Array[Int])(implicit f: Factor[A,B,_]): A = f.condition(a,variables,values)

  def marginalize[A,B,C](a: A,
                         variables: Array[Int],
                         domains: Array[Array[Int]])(implicit monoid: Monoid[B], f: Factor[A,B,C]): C =
    f.marginalize(a,variables,domains)

  def marginalizeDense[A,B](a: A,
                            vars: Array[Int],
                            doms: Array[Array[Int]])(implicit monoid: Monoid[B], manifestB: ClassManifest[B], f: Factor[A,B,_]): DenseFactor[B] =
    f.marginalizeDense(a,vars,doms)
}