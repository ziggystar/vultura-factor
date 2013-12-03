package vultura.util

/**
 * Pretty prints ASCII tables.
 *
 * User: Thomas Geier
 * Date: 06.10.11
 */

object TableFormatter {
  def table1[A](as: Seq[A],
                f: A => String,
                pa: A => String = (a: A) => a.toString): String = {
    val sizeA = as.map(_.toString.size).max
    val sizeF = as.map(f(_).size).max
    val fString = "%%%ds %%%ds".format(sizeA, sizeF)
    val rows = for (a <- as; v = f(a)) yield fString.format(a.toString, v)
    rows.mkString("\n")
  }

  /**
   * @param as the row indices.
   * @param bs the column indices.
   * @param f maps row,column to a content of a cell.
   * @param pa convert row indices to String.
   * @param pb convert column indices to String.
   */
  def table2[A, B](as: Seq[A],
                   bs: Seq[B])(
    f: (A, B) => String,
    pa: A => String = (a: A) => a.toString,
    pb: B => String = (b: B) => b.toString): String = {
    val sizeA = as.map(pa(_).size).max
    val colSizes = for (
      b <- bs;
      fMax = as.map(a => f(a, b).size).max;
      max = math.max(pb(b).size, fMax)
    ) yield max

    val fString = (sizeA +: colSizes).map("%%%ds".format(_)).mkString(" ")
    val firstRow = fString.format("" +: bs.map(pb(_)): _*)
    val dataRows = as.map(a => fString.format(pa(a) +: bs.map(b => f(a, b)): _*))
    (firstRow +: dataRows).mkString("\n")
  }
}