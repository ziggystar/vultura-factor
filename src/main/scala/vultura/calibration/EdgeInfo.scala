package vultura.calibration

/** Holds information wrt a single edge within a computation graph.
  * @param totalUpdates Number of total updates this edge has received.
  * @param lastUpdate Iteration during which the last update was made.
  * @param lastDiff Difference between last and second to last update.
  * @param totalDiff Sum of differences over all updates of this edge.
  */
case class EdgeInfo(totalUpdates: Long, lastUpdate: Long, lastDiff: Double, totalDiff: Double)
