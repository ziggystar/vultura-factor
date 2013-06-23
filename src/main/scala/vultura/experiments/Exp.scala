package vultura.experiments

import Reporter._
import scala.concurrent.ExecutionContext
import scala.collection.generic.FilterMonadic

/**
 * @author Thomas Geier
 * @since 6/21/13
 */

trait Exp[B] { outer =>
  protected def generator: Iterable[(B,Reporter[B])]

  def create: (Seq[String],Iterator[Seq[String]]) = {
    def createString(brb: (B,Reporter[B])) = brb._2.eval(brb._1)
    val it = generator.iterator
    val first = it.next()
    val header = first._2.colNames
    (header, Iterator(Iterator(createString(first)),it.map(createString)).flatten)
  }

  def map[C](f: B => C): Exp[C] = Exp(
    for{
      (b,rb) <- generator.view
      c = f(b)
    } yield (c,rb.hold(b): Reporter[C])
  )

  def withFilter(p: B => Boolean): Exp[B] = Exp(generator.view.withFilter(brb => p(brb._1)))

  def flatMap[C](f: B => Exp[C]): Exp[C] = Exp(
    for{
      (b,rb) <- outer.generator.view
      (c,rc) <- f(b).generator
    } yield (c,rb.hold(b) + rc: Reporter[C])
  )
  def take(n: Int): Exp[B] = Exp(generator.view.take(n))
  def withReport(r: Reporter[B]) = Exp(generator.view.map{case (b,rb) => (b,rb also r)})
  def addColumn(name: String, value: B => String): Exp[B] = withReport(Reporter(name)(value))

  /** Currently only supports flatMap calls. */
  //TODO support map calls
  def parallel(chunkSize: Int): Exp[B] = new Exp[B]{ inner =>
    protected def generator: Iterable[(B, Reporter[B])] = outer.generator

    override def map[C](f: (B) => C): Exp[C] = ???
    override def withFilter(p: (B) => Boolean): Exp[B] = outer.withFilter(p).parallel(chunkSize)
    override def flatMap[C](f: (B) => Exp[C]): Exp[C] = Exp.fromIteratorWithR(generator.grouped(chunkSize)
      .flatMap{ chunk =>
        chunk.par.map{ case (b,rb) =>
          f(b).generator.view.map{case (c,rc) =>  (null.asInstanceOf[C],(rb.hold(b): Reporter[C]) also (rc.hold(c): Reporter[C]))}.toIndexedSeq
        }
      }.flatten
    )
  }
}

object Exp {
  def apply(): Exp[Unit] = Exp[Unit](Iterable((Unit,Reporter.empty)))
  def apply[A](it: Iterable[(A,Reporter[A])]): Exp[A] = new Exp[A]{
    protected def generator: Iterable[(A, Reporter[A])] = it.view
  }
  def fromIteratorWithR[A](it: => Iterator[(A,Reporter[A])]): Exp[A] = Exp(
    new Iterable[(A, Reporter[A])]{
      override def withFilter(p: ((A, Reporter[A])) => Boolean): FilterMonadic[(A, Reporter[A]), Iterable[(A, Reporter[A])]] = this
      def iterator: Iterator[(A, Reporter[A])] = it
    }
  )
  def fromIterable[A](i: Iterable[A]): Exp[A] = Exp(i.view.map(_ -> (Reporter.empty: Reporter[A])))
  def fromIterator[A](i: => Iterator[A]): Exp[A] = Exp.fromIterable(new Iterable[A]{
    def iterator: Iterator[A] = i
  })
  def values[A](vals: A*): Exp[A] = Exp(vals.map(a => a -> (Reporter.empty: Reporter[A])))
  def seed(start: Long, number: Int, name: String = "seed"): Exp[Long] =
    Exp.values(Seq.iterate(start,number)(_ + 1):_*)
      .addColumn(name,_.toString)
}