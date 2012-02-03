package vultura.util

import org.specs2._
import specification.Fragments

class DomainCPITest extends Specification {
  val cpi1 = new DomainCPI(Seq(Array(1, 2), Array(3)))
  val cpi2 = new DomainCPI(Seq(Array(1, 2), Array(3, 4)))
  val cpiEmpty = new DomainCPI[Int](Nil :: Nil)

  def is: Fragments =
    (cpi1.iterator.flatten.toSeq === Seq(1, 3, 2, 3)) ^
    (cpi2.iterator.flatten.toSeq === Seq(1, 3, 2, 3, 1, 4, 2, 4)) ^
    "test empty CPI" ! (cpiEmpty.size === 0)
}