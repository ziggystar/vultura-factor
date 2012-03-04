package vultura.factors.misc

/**
 * @author Thomas Geier
 * @since 29.02.12
 */

case class NumberingScheme[A](numbering: Map[A,Int], nextFree: Int) {
  def register(elem: A*): NumberingScheme[A] = {
    val newElems = elem.filterNot(numbering.contains)
    val newEntries: Seq[(A, Int)] = newElems.zipWithIndex.map(t => (t._1,t._2 + nextFree))
    NumberingScheme(numbering ++ newEntries, nextFree + newElems.size)
  }
}

object NumberingScheme {
  def empty[A]: NumberingScheme[A] = NumberingScheme[A](Map.empty,0)
}