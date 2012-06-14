package vultura

import scalaz.Monoid
import collection.mutable.WrappedArray
import scala.util.Random
import util.Measure

/**
 * @author Thomas Geier
 * @since 03.02.12
 */

package object factors {
  def variables[A](a: A)(implicit f: Factor[A,B] forSome {type B}): Array[Int] = f.variables(a)
  def domains[A](a: A)(implicit f: Factor[A,_]): Array[Array[Int]] = f.domains(a)
  def evaluate[A,B](a: A, assignment: Array[Int])(implicit f: Factor[A,B]): B = f.evaluate(a,assignment)
  def condition[A: Conditionable](a: A, variables: Array[Int], values: Array[Int]): A = implicitly[Conditionable[A]].condition(a,variables,values)
  def sample[A,B](a: A, random: Random, m: Measure[B])(implicit f: Factor[A,B], cm: ClassManifest[B]): Option[Array[Int]] = f.sample(a,random, m)
  def partition[A,B](a: A,sumMonoid: Monoid[B])(implicit f: Factor[A,B],cm: ClassManifest[B]): B = f.partition(a,sumMonoid)
  def marginalizeDense[A,B](a: A,
                            vars: Array[Int],
                            doms: Array[Array[Int]],
                            monoid: Monoid[B])(implicit manifestB: ClassManifest[B], f: Factor[A,B]): TableFactor[B] =
    TableFactor.marginalizeDense(a,vars,doms,monoid)

  /** The domains to marginalize over are assumed to be the complete domains of the variables. */
  def marginalizeAllDense[A,B](a: A,
                              _vars: Set[Int],
                              monoid: Monoid[B])(implicit manifestB: ClassManifest[B], f: Factor[A,B]): TableFactor[B] = {
    val varDom = variables(a).zip(domains(a)).toMap
    val (vars,doms) = _vars.toSeq.map(v => (v,varDom(v))).unzip
    marginalizeDense(a, vars.toArray, doms.toArray, monoid)
  }

  /** Support for wrapping factors. */
  implicit def eitherFactor[A,B,R](implicit evA: Factor[A,R], evB: Factor[B,R]): Factor[Either[A,B],R] = new DenseFactor[Either[A,B],R]{
    def variables(f: Either[A, B]): Array[Int] = f.fold(evA.variables, evB.variables)
    def domains(f: Either[A, B]): Array[Array[Int]] = f.fold(evA.domains,evB.domains)
    def evaluate(f: Either[A, B], assignment: Array[Int]): R =
      f.fold(evA.evaluate(_,assignment),evB.evaluate(_,assignment))
  }

  implicit def eitherConditionable[A: Conditionable,B: Conditionable] = new Conditionable[Either[A,B]]{
    def condition(f: Either[A, B], variables: Array[Int], values: Array[Int]): Either[A, B] =
      f.left.map(factors.condition(_,variables,values)).right.map(factors.condition(_,variables,values))
  }

  /** Support for wrapping factors. */
  implicit def eitherFactorSparse[A,B,R](implicit evA: SparseFactor[A,R], evB: SparseFactor[B,R]): SparseFactor[Either[A,B],R] = new SparseFactor[Either[A,B],R]{
    def variables(f: Either[A, B]): Array[Int] = f.fold(evA.variables, evB.variables)
    def domains(f: Either[A, B]): Array[Array[Int]] = f.fold(evA.domains,evB.domains)
    def defaultValue(f: Either[A, B]): R = f.fold(evA.defaultValue,evB.defaultValue)
    def points(f: Either[A, B]): Map[WrappedArray[Int], R] = f.fold(evA.points,evB.points)
  }
}