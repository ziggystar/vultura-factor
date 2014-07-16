package vultura.factor.inference.calibration


trait Diff[-E, -T] {
  def diff(e: E)(oldValue: T, newValue: T): Double
}

object MaxDiff extends Diff[Any, Array[Double]] {
  override def diff(e: Any)(oldValue: Array[Double], newValue: Array[Double]): Double =
    vultura.util.maxDiff(oldValue, newValue)
}
