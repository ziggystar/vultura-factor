package vultura.factors

import vultura.{factors => vf}

class FactorView[A](val condition: Map[Int, Int], val factor: A)(implicit evF: Factor[A, _]) {

  val variables = vf.variables(factor).filterNot(condition.contains)
  val domains = variables.map(vf.variables(factor).zip(vf.domains(factor)).toMap)
}

object FactorView{


  implicit def factorTC[A,R](implicit evF: Factor[A, R]) = new DenseFactor[FactorView[A],R]{

    def variables(f: FactorView[A]): Array[Int] = f.variables
    def domains(f: FactorView[A]): Array[Array[Int]] = f.domains
    def evaluate(f: FactorView[A], assignment: Array[Int]) =
        vf.evaluate(f.factor, f.variables.map((f.variables zip assignment).toMap ++ f.condition))
      def condition(f: FactorView[A], variables: Array[Int], values: Array[Int]): FactorView[A] =
        new FactorView(f.condition ++ (f.variables zip values).toMap, f.factor)
  }
}