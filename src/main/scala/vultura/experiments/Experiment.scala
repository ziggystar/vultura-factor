package vultura.experiments

import Reporter._

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
  def take(i: Int) = Experiment.fromIteratorWithReport(iterator.take(i))
  def run(printHeader: Boolean = true): Seq[String] = {
    var firstLine = printHeader

    iterator.flatMap{ case (a, ra) =>
      if(firstLine){
        firstLine = false
        Seq(ra.header,ra.buildRow(a))
      }
      else
        Seq(ra.buildRow(a))
    }.toIndexedSeq
  }
  def withReport(r: Reporter[A]): Experiment[A] = Experiment.fromIteratorWithReport(iterator.map{case ((a,ra)) => (a,ra.also(r))})
}

object Experiment{
  def apply[A](a: A): Experiment[A] = fromIterator(Iterator(a))
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