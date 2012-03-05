package vultura.graph

import org.specs2._
import specification.Fragments

import GraphOps._

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 18.10.11
 */

class GraphTest extends Specification {
  import TupleSetGraph._

  import scalaz._
  import Scalaz._

  def is: Fragments =
  "decomposition into components" ^
  (Set((1,2),(3,4)) |> (s => s + " has Set(1,2) as component of 1" ! (componentOf(s,1) must_== Set(1,2)))) ^
  (Set((1,2),(3,4)) |> (s => s + " has Set(1,2) as component of 2" ! (componentOf(s,2) must_== Set(1,2)))) ^
  (Set((1,2),(2,3)) |> (s => s + " has Set(1,2,3) as component of 1" ! (componentOf(s,1) must_== Set(1,2,3)))) ^
  (Set((1,2),(2,3),(4,5)) |> (s => s + " has Set(4,5) as component of 4" ! (componentOf(s,4) must_== Set(4,5)))) ^
  ((Set((1,2),(3,4)) |> (s => s + " has components (1,2) and (3,4)" ! (components(s) must_== s.map(t => Set(t._1,t._2)))))) ^
  ((Set((1,2),(2,3),(3,4)) |> (s => s + " has component (1,2,3,4)" ! (components(s) must_== Set(Set(1,2,3,4)))))) ^
  p^
  "cycle tests" ^
  "empty graph is a tree" ! (isTree(Set.empty[(Int,Int)]) must beTrue) ^
  "(1,2),(2,3),(2,4) is a tree".! (isTree(Set((1, 2), (2, 3), (2, 4))) must beTrue) ^
  (Set((1,2),(2,3),(2,4),(1,4)) |> (s => s + " is not a tree" ! (isTree(s) must beFalse))) ^
  (Set((1,2),(2,3),(4,5),(5,6),(6,4)) |> (s => s + " is not a tree (cycle in second component" ! (isTree(s) must beFalse))) ^
  (Set((1,1)) |> (s => s + " is not a tree (self cycle)" ! (isTree(s) must beFalse)))

  def isTree(g: Set[(Int, Int)]): Boolean = !containsCycle(g)
}