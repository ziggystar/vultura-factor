package vultura.util

import org.specs2._
import specification.Fragments
import TreeWidth._

class TreeWidthTest extends Specification {
  def is: Fragments =
  (treeWidth(List(Set(1,2),Set(2,3),Set(3,4),Set(1,4)),List(1,2,3,4)) === 2) ^
  (treeWidth(List(Set(1,2),Set(2,3)), List(1,2,3)) === 1)
}