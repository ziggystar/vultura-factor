package vultura.fastfactors

import org.specs2._
import org.specs2.specification.Fragments

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 4/9/13
 */
class LogD$Test extends Specification {
  import math.{log,exp}

  def is: Fragments =
    "simple sum" ! (LogD.sum(log(3),log(4)) must beCloseTo(log(7),0.01)) ^
    "simple sum (swap)" ! (LogD.sum(log(4),log(3)) must beCloseTo(log(7),0.01)) ^
    "simple prod" ! (LogD.prod(log(4),log(3)) must beCloseTo(log(12),0.01)) ^
    "array sum" ! (LogD.sumA(Array(log(1),log(2),log(0.5))) must beCloseTo(log(3.5),0.01)) ^
    "array prod" ! (LogD.prodA(Array(log(1),log(2),log(0.5))) must beCloseTo(log(1),0.01))
}
