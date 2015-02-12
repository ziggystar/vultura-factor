package vultura.factor

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor.generators._

import scala.util.Random

class SumProductPowTaskTest extends Specification with FactorMatchers {
  override def is: Fragments =
    "identity operation" ! {
      val domains: Array[Int] = Array(2, 2)
      val scope: Array[Var] = Array(0, 1)
      val f = expGauss().apply(scope,domains, new Random(0))
      SumProductPowTask.runOnce(domains,scope,NormalD,Seq((1d,Seq(f)))) must beSimilarTo(f,1e-12)
    } ^
    "factor division by self" ! {
      val domains: Array[Int] = Array(2, 2)
      val scope: Array[Var] = Array(0, 1)
      val f = expGauss().apply(scope,domains, new Random(0))
      SumProductPowTask.runOnce(domains,scope,NormalD,Seq((1d,Seq(f)),(-1d,Seq(f)))) must beSimilarTo(Factor.neutral(scope,domains,NormalD),1e-12)
    } ^
    "normal sum-product of two factors" ! {
      val domains: Array[Int] = Array(2, 2, 2)
      val scope1: Array[Var] = Array(0, 1)
      val scope2: Array[Var] = Array(1, 2)
      val f1 = expGauss().apply(scope1,domains,new Random(0))
      val f2 = expGauss().apply(scope2,domains,new Random(1))
      SumProductPowTask.runOnce(domains,Array(1),NormalD,Seq((1d,Seq(f1,f2)))) must
        beSimilarTo(Factor.multiplyRetain(NormalD)(domains)(Array(f1,f2),Array(1)),1e-12)
    }
}
