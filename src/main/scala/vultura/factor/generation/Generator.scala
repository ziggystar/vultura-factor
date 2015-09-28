package vultura.factor.generation

import scala.util.Random

/** A probability monad.
  * Basically a distribution over values of type `A`. */
trait Generator[+A] { outer =>
  def generate(r: Random): A
  def map[B](f: A => B): Generator[B] = new Generator[B]{
    override def generate(r: Random): B = f(outer.generate(r))
  }
  def flatMap[B](f: A => Generator[B]): Generator[B] = new Generator[B]{
    override def generate(r: Random): B = f(outer.generate(r)).generate(r)
  }
  def withFilter(p: A => Boolean): Generator[A] = new Generator[A] {
    def generate(r: Random): A = {
      var res: A = null.asInstanceOf[A]
      do {
        res = outer.generate(r)
      } while(!p(res))
      res
    }
  }
}

object Generator {
  def apply[A](f: Random => A): Generator[A] = new Generator[A]{
    override def generate(r: Random): A = f(r)
  }
  def seq[X](xs: Seq[Generator[X]]): Generator[Seq[X]] = Generator(r => xs.map(_.generate(r)))
}

case class Constant[+A](a: A) extends Generator[A]{
  override def generate(r: Random): A = a
}