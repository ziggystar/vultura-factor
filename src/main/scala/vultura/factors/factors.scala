package vultura

import scalaz.Monoid
import collection.mutable.WrappedArray
import scala.util.Random
import util.Measure
import collection.immutable.Iterable

/**
 * @author Thomas Geier
 * @since 03.02.12
 */

package object factors {
  def variables[A](a: A)(implicit f: Factor[A,_]): Array[Int] = f.variables(a)
  def domains[A](a: A)(implicit f: Factor[A,_]): Array[Array[Int]] = f.domains(a)
  def evaluate[A,B](a: A, assignment: Array[Int])(implicit f: Factor[A,B]): B = f.evaluate(a,assignment)
  def condition[A,B](a: A, variables: Array[Int], values: Array[Int])(implicit f: Factor[A,B]): A = f.condition(a,variables,values)
  def sample[A,B](a: A, random: Random)(implicit f: Factor[A,B], m: Measure[B]): Array[Int] = f.sample(a,random)

  def marginalize[A,B,C](a: A,
                         variables: Array[Int],
                         domains: Array[Array[Int]])(implicit monoid: Monoid[B], f: Factor[A,B], tm: TransMarginalize[A,B,C]): C =
    tm.marginalize(a,variables,domains)

  def marginalizeDense[A,B](a: A,
                            vars: Array[Int],
                            doms: Array[Array[Int]])(implicit monoid: Monoid[B], manifestB: ClassManifest[B], f: Factor[A,B]): TableFactor[B] =
    TableFactor.marginalizeDense(a,vars,doms)

  /** sumMonoid needs to distribute over product.productMonoid. */
  def eliminateDense[F,R](product: ProductFactor[Either[F,TableFactor[R]],R],
                          eliminationVariable: Int,
                          sumMonoid: Monoid[R])(implicit ev: Factor[F,R],
                                                cmr: ClassManifest[R]): ProductFactor[Either[F,TableFactor[R]],R] = {
    val (affected,unaffected) = product.factors.partition(variables(_).contains(eliminationVariable))
    val affectedProduct = ProductFactor(affected, product.productMonoid)
    val resultingFactor: TableFactor[R] = TableFactor.marginalizeDense(
      affectedProduct,
      Array(eliminationVariable),
      Array(domains(affectedProduct).apply(variables(affectedProduct).indexOf(eliminationVariable)))
    )(implicitly[Factor[ProductFactor[Either[F,TableFactor[R]],R],R]],sumMonoid,cmr)
    ProductFactor(unaffected.toIndexedSeq :+ Right(resultingFactor),product.productMonoid)
  }

  /** Support for wrapping factors. */
  implicit def eitherFactor[A,B,R](implicit evA: Factor[A,R], evB: Factor[B,R]): Factor[Either[A,B],R] = new DenseFactor[Either[A,B],R]{
    def variables(f: Either[A, B]): Array[Int] =
      f.fold(evA.variables, evB.variables)
    def domains(f: Either[A, B]): Array[Array[Int]] =
      f.fold(evA.domains,evB.domains)
    def evaluate(f: Either[A, B], assignment: Array[Int]): R =
      f.fold(evA.evaluate(_,assignment),evB.evaluate(_,assignment))
    def condition(f: Either[A, B], variables: Array[Int], values: Array[Int]): Either[A, B] =
      f.left.map(evA.condition(_,variables,values)).right.map(evB.condition(_,variables,values))
  }

  /** Support for wrapping factors. */
  implicit def eitherFactorSparse[A,B,R](implicit evA: SparseFactor[A,R], evB: SparseFactor[B,R]): SparseFactor[Either[A,B],R] = new SparseFactor[Either[A,B],R]{
    def variables(f: Either[A, B]): Array[Int] =
      f.fold(evA.variables, evB.variables)
    def domains(f: Either[A, B]): Array[Array[Int]] =
      f.fold(evA.domains,evB.domains)
    def condition(f: Either[A, B], variables: Array[Int], values: Array[Int]): Either[A, B] =
      f.left.map(evA.condition(_,variables,values)).right.map(evB.condition(_,variables,values))
    def defaultValue(f: Either[A, B]): R = f.fold(evA.defaultValue,evB.defaultValue)
    def points(f: Either[A, B]): Map[WrappedArray[Int], R] = f.fold(evA.points,evB.points)
  }
}