package vultura.calibration

trait ExternalBuilder[CP <: CalProblem, R] {
  def build(cp: CP)(valuation: cp.N => cp.IR): R

  def apply(calibrator: Calibrator[CP]): R = build(calibrator.cp)(calibrator.nodeState)
}

object ExternalBuilder {
  def fromBuilder[R]: ExternalBuilder[CalProblem with ResultBuilder[R], R] =
    new ExternalBuilder[CalProblem with ResultBuilder[R],R] {
      override def build(cp: CalProblem with ResultBuilder[R])(valuation: (cp.N) => cp.IR): R = cp.buildResult(valuation)
    }
}
