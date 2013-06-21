package vultura.experiments

import Reporter._
import scala.concurrent.ExecutionContext

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
  def withReport(r: Reporter[B]) = Exp(generator.view.map{case (b,rb) => (b,rb also r)})
  def addColumn(name: String, value: B => String): Exp[B] = withReport(Reporter(name)(value))
  def parallelize(chunkSize: Int): Exp[B] = ???
}

object Exp {
  def apply(): Exp[Unit] = Exp[Unit](Iterable((Unit,Reporter.empty)))
  def apply[A](it: Iterable[(A,Reporter[A])]): Exp[A] = new Exp[A]{
    require(!it.isEmpty, "creating empty experiment")
    protected def generator: Iterable[(A, Reporter[A])] = it
  }
  def values[A](vals: A*): Exp[A] = Exp(vals.map(a => a -> (Reporter.empty: Reporter[A])))
}