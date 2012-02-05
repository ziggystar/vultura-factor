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

  /** Support for wrapping factors. */
  implicit def eitherFactor[A,B,R,M](implicit evA: Factor[A,R,M], evB: Factor[B,R,M]): Factor[Either[A,B],R,M] = new Factor[Either[A,B],R,M]{
    def variables(f: Either[A, B]): Array[Int] =
      f.fold(evA.variables, evB.variables)
    def domains(f: Either[A, B]): Array[Array[Int]] =
      f.fold(evA.domains,evB.domains)
    def evaluate(f: Either[A, B], assignment: Array[Int]): R =
      f.fold(evA.evaluate(_,assignment),evB.evaluate(_,assignment))
    def condition(f: Either[A, B], variables: Array[Int], values: Array[Int]): Either[A, B] =
      f.left.map(evA.condition(_,variables,values)).right.map(evB.condition(_,variables,values))
    def marginalize(f: Either[A, B], variables: Array[Int], domains: Array[Array[Int]])(implicit monoid: Monoid[R]): M =
      f.fold(evA.marginalize(_,variables,domains)(monoid),evB.marginalize(_,variables,domains)(monoid))
  }
}