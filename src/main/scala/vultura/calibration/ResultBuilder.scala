package vultura.calibration

trait ResultBuilder[+R] {outer: CalProblem =>
  def buildResult(valuation: outer.N => IR): R
}
