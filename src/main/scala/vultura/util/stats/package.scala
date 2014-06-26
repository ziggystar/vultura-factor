package vultura.util

/**
 * Created by thomas on 20.06.14.
 */
package object stats {
  implicit class RichNumericVector[A](val xs: Iterable[A]) extends AnyVal {
    def mean(implicit ev: Numeric[A]): Double = ev.toDouble(xs.sum) / xs.size
    def variance(implicit ev: Numeric[A]): Double = {
      if (xs.size == 1) return 0
      val mean_s: Double = mean
      xs.map(ev.toDouble(_) - mean_s).map(x => x * x).sum / (xs.size - 1)
    }
    def sd(implicit ev: Numeric[A]): Double = math.sqrt(variance)
    def median(implicit ev: Ordering[A]): A = quantile(.5)
    def quantile(q: Double)(implicit ev: Ordering[A]): A = xs.toIndexedSeq.sorted.apply(math.max((xs.size - 1) * q, xs.size - 1).toInt)
  }

}
