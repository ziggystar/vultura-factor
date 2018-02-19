package vultura.calibration

case class ConvergenceStats(iterations: Long, maxDiff: Double, isConverged: Boolean) {
  def max(other: ConvergenceStats) = ConvergenceStats(
    iterations max other.iterations,
    maxDiff max other.maxDiff,
    isConverged && other.isConverged
  )
}

object ConvergenceStats {
  def exact: ConvergenceStats = ConvergenceStats(1, 0d, isConverged = true)
}