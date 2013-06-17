package vultura.experiments

import Reporter._
import java.io.{PrintStream, OutputStreamWriter, OutputStream}

/**
 * @author Thomas Geier
 * @since 6/16/13
 */

trait Experiment[A]{
  def iterator: Iterator[(A,Reporter[A])]

  def map[B](f: A => B): Experiment[B] = Experiment.fromIteratorWithReport(iterator.map{case (a,ra) => (f(a),ra.hold(a))})
  def filter(f: A => Boolean): Experiment[A] = Experiment.fromIteratorWithReport(iterator.filter(ara => f(ara._1)))
  def flatMap[B](f: A => Experiment[B]): Experiment[B] = Experiment.fromIteratorWithReport(
    for(
      (a,ra) <- iterator;
      (b,rb) <- f(a).iterator
    ) yield (b,ra.hold(a) + rb))
  def run(os: PrintStream) {
    val buffered = iterator.buffered
    os.println(buffered.head._2.header)
    buffered.foreach{ case (a, ra) =>
      os.println(ra.buildRow(a))
    }
  }
  def withReport(r: Reporter[A]): Experiment[A] = Experiment.fromIteratorWithReport(iterator.map{case ((a,ra)) => (a,ra.also(r))})
}

object Experiment{
  def apply[A](a: A): Experiment[A] = new Experiment[A] {
    def iterator = Iterator((a,Reporter.empty))
  }
  def fromIteratorWithReport[A](as: Iterator[(A,Reporter[A])]): Experiment[A] = new Experiment[A] {
    def iterator: Iterator[(A, Reporter[A])] = as
  }

  def fromIterator[A](as: Iterator[A]): Experiment[A] = new Experiment[A] {
    def iterator: Iterator[(A, Reporter[A])] = as.map((_,Reporter.empty))
  }

  def generateSeed(name: String)(start: Long, number: Int) = {
    require(number > 0)
    Experiment.fromIteratorWithReport(Iterator.iterate(start){_ + 1}.take(number).map(i => (i,Reporter(name)((_: Long).toString))))
  }
  def description(name: String)(value: String): Experiment[Unit] =
    Experiment.fromIteratorWithReport(Iterator((Unit,Reporter.constant(name,value))))
}