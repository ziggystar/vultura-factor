package vultura.util.random

import scala.annotation.tailrec

/** A probability monad.
  * Basically a distribution over values of type `A`. */
trait Dist[+A] extends (MutableRng => A) { outer =>
  def apply(rng: MutableRng): A
  def repeat(n: Int): Dist[IndexedSeq[A]] = Dist(r => IndexedSeq.fill(n)(outer(r)))
  def map[B](f: A => B): Dist[B] = Dist(f compose outer)
  def flatMap[B](f: A => Dist[B]): Dist[B] = Dist(r => (f compose outer)(r)(r))
  def withFilter(p: A => Boolean): Dist[A] = Dist{ r =>
    var res: A = null.asInstanceOf[A]
    do {
      res = outer(r)
    } while(!p(res))
    res
  }
}

object Dist {
  def apply[A](f: MutableRng => A) = new Dist[A]{
    override def apply(rng: MutableRng): A = f(rng)
  }
  def seq[X](xs: Seq[Dist[X]]): Dist[Seq[X]] = Dist(r => xs.map(_(r)))
  def gaussian(mean: Double = 0d, sd: Double = 1d): Dist[Double] = Dist(_.nextGaussian() * sd + mean)

  /** @param lower Lower, inclusive bound.
    * @param upper Upper, exclusive bound.
    * @return Uniformly distributed double within given array.
    */
  def uniform(lower: Double = 0d, upper: Double = 1d): Dist[Double] = Dist(_.nextDouble() * (upper - lower) + lower)

  /** @param p Probability of one. */
  def bernoulli(p: Double): Dist[Boolean] = Dist(_.nextDouble() < p)

  def multinomial[A](values: (A,Double)*): Dist[A] = {
    val normal = values.map{v =>
      require(v._2 >= 0, "encountered negative probability")
      v._2
    }.sum
    val probs: Array[Double] = values.map(_._2)(collection.breakOut)
    val results: IndexedSeq[A] =  values.map(_._1)(collection.breakOut)
    Dist{ r =>
      val needle = r.nextDouble() * normal
      @tailrec def find(i: Int = 0, cum: Double = 0): A = {
        val next = cum + probs(i)
        if(needle < next) results(i)
        else find(i+1,next)
      }
      find()
    }
  }
}

/** Mutable pseudo-RNG type-class with seed type `S`.*/
trait MutableRng {
  /** @return Uniform int in range [0,max[. */
  def nextInt(max: Int): Int
  def nextLong(): Long
  /** @return Random double in range [0,1[. */
  def nextDouble(): Double
  /** @return normally distributed double; mean 0, sd 1. */
  def nextGaussian(): Double
  def nextBoolean(): Boolean
}

object MutableRng {
  import java.util.{Random => JRandom}

  import scala.util.{Random => SRandom}

  implicit class ScalaRandomMutableRng(random: SRandom) extends MutableRng {
    /** @return Uniform int in range [0,max[. */
    override def nextInt(max: Int): Int = random.nextInt(max)
    override def nextBoolean(): Boolean = random.nextBoolean()
    /** @return Random double in range [0,1[. */
    override def nextDouble(): Double = random.nextDouble()
    /** @return normally distributed double; mean 0, sd 1. */
    override def nextGaussian(): Double = random.nextGaussian()
    override def nextLong(): Long = random.nextLong()
  }

  implicit class JavaRandomMutableRng(random: JRandom) extends MutableRng {
    /** @return Uniform int in range [0,max[. */
    override def nextInt(max: Int): Int = random.nextInt(max)
    override def nextBoolean(): Boolean = random.nextBoolean()
    /** @return Random double in range [0,1[. */
    override def nextDouble(): Double = random.nextDouble()
    /** @return normally distributed double; mean 0, sd 1. */
    override def nextGaussian(): Double = random.nextGaussian()
    override def nextLong(): Long = random.nextLong()
  }
}
