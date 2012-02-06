package vultura.factors

class FactorView[A, R](val condition: Map[Int, Int], val factor: A)(implicit evF: Factor[A, R]) {

  import vultura.{factors => vf}

  val variables =
    vf.variables(factor).filterNot(condition.contains)
  val domains =
    variables.map(vf.variables(factor).zip(vf.domains(factor)).toMap)
  def evaluate(assignment: Array[Int]) =
    vf.evaluate(factor, variables.map((variables zip assignment).toMap ++ condition))
  def condition(variables: Array[Int], values: Array[Int]): FactorView[A, R] =
    new FactorView(condition ++ (variables zip values).toMap, factor)(evF)
}