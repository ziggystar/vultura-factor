package vultura.fastfactors

import org.specs2._
import org.specs2.specification.Fragments
import Utils._

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 4/9/13
 */
class LogD$Test extends Specification {
  import math.{log,exp}

  def is: Fragments =
    "simple sum" ! (LogD.sum(log(3),log(4)) must beCloseTo(log(7),0.01)) ^
    "sum with -Inf" ! (LogD.sum(Double.NegativeInfinity, 5) must beCloseTo(5,0.01)) ^
    "sum with -Inf (2)" ! (LogD.sum(-5, Double.NegativeInfinity) must beCloseTo(-5,0.01)) ^
    "simple sum (swap)" ! (LogD.sum(log(4),log(3)) must beCloseTo(log(7),0.01)) ^
    "simple prod" ! (LogD.prod(log(4),log(3)) must beCloseTo(log(12),0.01)) ^
    "array sum" ! (LogD.sumA(Array(log(1),log(2),log(0.5))) must beCloseTo(log(3.5),0.01)) ^
    "array sum with -Inf" ! (LogD.sumA(Array.fill(3)(Double.NegativeInfinity)) === Double.NegativeInfinity) ^
    "array prod" ! (LogD.prodA(Array(log(1),log(2),log(0.5))) must beCloseTo(log(1),0.01)) ^
    "max norm" ! (LogD.maxNorm(AD(log(1),log(2)),AD(log(5),log(3))) must beCloseTo(4,1e-5)) ^
    "normalization" ! (LogD.normalize(AD(log(1),log(2),log(3))).map(exp).sum must be closeTo(1d,1e-5))

}
