package vultura.factor

import org.specs2._

class SumProductTaskTest extends Specification {
  override def is =
    "multiply the same factor(2,2) twice and marginalize one variable out" ! {
      val f = Factor(Array(0,1),Array(0d,1,2,3))
      val spt = SumProductTask(Array(0),Array(2,2),Array(Array(0,1),Array(0,1)),NormalD)
      val result = new Array[Double](2)
      spt.sumProduct(IndexedSeq(f.values,f.values),result)
      result.deep === Array(4d,10).deep
    }
}
