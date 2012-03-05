package vultura.graph

import org.specs2._
import specification.Fragments

import TupleSetGraph._
import GraphOps._
import scalaz._
import Scalaz._

/**
  * Created by IntelliJ IDEA.
  * User: Thomas Geier
  * Date: 20.10.11 */

class TreeWidthTest extends Specification {

  def is: Fragments =
  "test for correct tree width of some graphs" ^
    treeWidthTest(Set((1,2)), 1) ^
    treeWidthTest(Set((1,2),(2,3)), 1) ^
    treeWidthTest(Set((1,2),(2,3),(2,4)), 1) ^
    treeWidthTest(Set((1,2),(2,3),(2,4),(3,4)), 2) ^
    treeWidthTest(Set((1,2),(2,3),(2,4),(3,4),(1,4)), 2) ^
  p^
  "test for correct tree decompositions" ^
    decompositionTest(Set((1,2),(2,3)), Set(Set(1,2),Set(2,3)))

  def treeWidthTest(graph: Set[(Int, Int)], tw: Int): Fragments =
    "%s has treewidth %d".format(graph,tw) ! (treeWidth(graph) must_== tw)
  def decompositionTest(input: Set[(Int,Int)], expectedVertSets: Set[Set[Int]]): Fragments =
    "%s has vertex sets %s".format(input, expectedVertSets) ! (treeDecomposition(input).map(_.flatten.toSet).head must_== expectedVertSets)
}