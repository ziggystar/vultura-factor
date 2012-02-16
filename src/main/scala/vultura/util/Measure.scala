package vultura.util

/**
 * Type class that says that provides a measure fpr values of type `A`.
 */

trait Measure[A] {
  /** @return a non--negative real */
  def weight(a: A*): Double
}

object Measure {
  implicit def numbersHaveMeasure[N: Numeric]: Measure[N] = new Measure[N] {
    def weight(a: N*): Double = a.distinct.map(implicitly[Numeric[N]].toDouble).sum
  }
}
