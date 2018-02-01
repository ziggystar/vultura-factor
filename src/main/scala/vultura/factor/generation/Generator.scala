package vultura.factor.generation

import org.apache.commons.math3.distribution.GammaDistribution

import scala.util.Random

/** A probability monad.
  * Basically a distribution over values of type `A`. */
trait Generator[+A] { outer =>
  def generate(r: Random): A
  def withSeed(s: Long = 0L): A = generate(new Random(s))
  def map[B](f: A => B): Generator[B] = (r: Random) => f(outer.generate(r))
  def flatMap[B](f: A => Generator[B]): Generator[B] = (r: Random) => f(outer.generate(r)).generate(r)
  def withFilter(p: A => Boolean, maxTries: Int = 1000): Generator[A] = (r: Random) => {
    var res: A = null.asInstanceOf[A]
    var tries = 0
    do {
      res = outer.generate(r)
      tries += 1
    } while (!p(res) && tries < maxTries)
    if(tries >= maxTries)
      sys.error(s"could not generate sample in filtered Generator; rejected maximal number of $maxTries samples")
    res
  }

  def replicate(n: Int): Generator[IndexedSeq[A]] = Generator(r => IndexedSeq.fill(n)(this.generate(r)))
}

object Generator {
  def apply[A](f: Random => A): Generator[A] = (r: Random) => f(r)
  def draw[A](as: Seq[A]): Generator[A] = (r: Random) =>
    as(r.nextInt(as.size.ensuring(_ > 0, "cannot sample from empty collection")))
  def seq[X](xs: Seq[Generator[X]]): Generator[Seq[X]] = Generator(r => xs.map(_.generate(r)))
  def gaussian(mean: Double = 0d, sd: Double = 1d): Generator[Double] = Generator(_.nextGaussian() * sd + mean)
  def gamma(shape: Double, scale: Double): Generator[Double] = new Generator[Double] {
    val gamma: GammaDistribution = new GammaDistribution(shape, scale)

    override def generate(r: Random): Double = {
      gamma.reseedRandomGenerator(r.nextLong)
      gamma.sample()
    }

    override def replicate(n: Int): Generator[IndexedSeq[Double]] = Generator { r =>
      gamma.reseedRandomGenerator(r.nextLong)
      gamma.sample(n).toIndexedSeq
    }
  }

  /** @param lower Lower, inclusive bound.
    * @param upper Upper, exclusive bound.
    * @return Uniformly distributed double within given array.
    */
  def uniform(lower: Double = 0d, upper: Double = 1d): Generator[Double] = Generator(_.nextDouble() * (upper - lower) + lower)

  /** @param p Probability of one. */
  def bernoulli(p: Double): Generator[Boolean] = Generator(_.nextDouble() < p)
  def only[A](a: A): Generator[A] = Constant(a)
}

case class Constant[+A](a: A) extends Generator[A]{
  override def generate(r: Random): A = a
}