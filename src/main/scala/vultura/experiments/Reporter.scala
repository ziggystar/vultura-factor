package vultura.experiments

/**
 * @author Thomas Geier
 * @since 6/16/13
 */

@deprecated("will be moved to separate library")
trait Reporter[-A]{ outer =>
  def header: String = prefix + colNames.mkString(separator)
  def buildRow(a: A): String = prefix + eval(a).mkString(separator)

  def separator: String = "\t"
  def prefix: String = ""
  def colNames: Seq[String]
  def eval(a: A): Seq[String]

  def +[B](other: Reporter[B]) = new Reporter[(A,B)]{
    def colNames: Seq[String] = Reporter.this.colNames ++ other.colNames
    def eval(ab: (A,B)): Seq[String] = Reporter.this.eval(ab._1) ++ other.eval(ab._2)
  }
  def also[B <: A](other: Reporter[B]) = new Reporter[B]{
    def colNames: Seq[String] = Reporter.this.colNames ++ other.colNames
    def eval(a: B): Seq[String] = Reporter.this.eval(a) ++ other.eval(a)
  }
  def comap[B](f: B => A): Reporter[B] = new Reporter[B] {
    def eval(a: B): Seq[String] = Reporter.this.eval(f(a))
    def colNames: Seq[String] = Reporter.this.colNames
  }
  def hold(a: A): Reporter[Unit] = new Reporter[Unit]{
    val preresult: Seq[String] = outer.eval(a)
    def colNames: Seq[String] = outer.colNames
    def eval(a: Unit): Seq[String] = preresult
  }
}

object Reporter{
  def apply[A](head: Seq[String])(report: A => Seq[String]) = new Reporter[A] {
    def colNames: Seq[String] = head
    def eval(a: A): Seq[String] = report(a)
  }
  def apply[A](head: String)(report: A => String) = new Reporter[A] {
    def colNames: Seq[String] = Seq(head)
    def eval(a: A): Seq[String] = Seq(report(a))
  }
  def constant(head: String, value: String) = new Reporter[Unit] {
    def colNames: Seq[String] = Seq(head)
    def eval(a: Unit): Seq[String] = Seq(value)
  }
  def empty: Reporter[Unit] = new Reporter[Unit] {
    def colNames: Seq[String] = Seq()
    def eval(a: Unit): Seq[String] = Seq()
  }
  implicit def absorbUnit1[A](r: Reporter[(Unit,A)]): Reporter[A] = r.comap((Unit,_))
  implicit def absorbUnit2[A](r: Reporter[(A,Unit)]): Reporter[A] = r.comap((_,Unit))
  implicit def unitToAny[A](r: Reporter[Unit]): Reporter[A] = r.comap(_ => Unit)
}