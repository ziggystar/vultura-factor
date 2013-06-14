package vultura.fastfactors

import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/14/13
 */
case class Problem(factors: IndexedSeq[FastFactor],domains: Array[Int],ring: RingZ[Double]){
  lazy val variables: Set[Int] = (for (f <- factors; v <- f.variables) yield v)(collection.breakOut)
  private lazy val degrees: mutable.HashMap[Int,Int] = new mutable.HashMap[Int,Int]
  def degreeOfVariable(v: Int): Int = degrees.getOrElseUpdate(v,factors.count(_.variables.contains(v)))
  def uaiString: String = {
    require(variables == Seq.range(0,variables.size).toSet)
    Seq[Any](
      "MARKOV",
      variables.size,
      domains.mkString(" "),
      factors.size,
      factors.map(f => f.variables.size + " " + f.variables.mkString(" ")).mkString("\n"),
      factors.map(f => f.values.size + " " + ring.decode(f.values).mkString(" ")).mkString("\n")
    ).mkString("\n")
  }
}
