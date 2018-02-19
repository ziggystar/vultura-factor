package vultura.factor

import org.specs2._
import vultura.factor.Utils._

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 4/9/13
 */
class LogDTest extends Specification {
  import math.{exp, log}

  def is =
    "simple sum" ! (LogD.sum(log(3),log(4)) must beCloseTo(log(7),0.01)) ^
    "sum with -Inf" ! (LogD.sum(Double.NegativeInfinity, 5) must beCloseTo(5,0.01)) ^
    "sum with -Inf (2)" ! (LogD.sum(-5, Double.NegativeInfinity) must beCloseTo(-5,0.01)) ^
    "simple sum (swap)" ! (LogD.sum(log(4),log(3)) must beCloseTo(log(7),0.01)) ^
    "simple prod" ! (LogD.prod(log(4),log(3)) must beCloseTo(log(12),0.01)) ^
    "array sum" ! (LogD.sumA(Array(log(1),log(2),log(0.5))) must beCloseTo(log(3.5),0.01)) ^
    "array sum with -Inf" ! (LogD.sumA(Array.fill(3)(Double.NegativeInfinity)) === Double.NegativeInfinity) ^
    "array prod" ! (LogD.prodA(Array(log(1),log(2),log(0.5))) must beCloseTo(log(1),0.01)) ^
    //"max norm" ! (LogD.maxNorm(AD(log(1),log(2)),AD(log(5),log(3))) must beCloseTo(4,1e-5)) ^
    "normalization" ! (LogD.normalize(AD(log(1),log(2),log(3))).map(exp).sum must be closeTo(1d,1e-5)) ^
    "expectation" ! (LogD.expectation(AD(log(0.2),log(0.8)),AD(10,20)) must be closeTo(18d,1e-5)) ^
    "entropy" ! (LogD.entropy(AD(log(0.2),log(0.8))) must be closeTo(0.5004d,1e-5)) ^
    "log expectation with zeros" ! (LogD.logExpectation(Array(Double.NegativeInfinity,0), Array(Double.NegativeInfinity,0)) === 0d)
}
