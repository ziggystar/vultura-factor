package vultura.fastfactors.algorithms.conditioned

import vultura.fastfactors.Problem
import vultura.util.DomainCPI
import scala.util.Random

/**
 * A simple conditioning scheme is defined by a set of variables, accompanied by a scope. The scope
 * must contain the variable itself.
 *
 * A (local) condition is specified by a set of variables (represented as integers). We assume that
 * these conditioning variables are fully split, each.
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
case class SimpleScheme(problem: Problem, splits: Set[(Set[Int],Int)]) {
  require(splits.map(_._2).subsetOf(problem.variables))
  require(splits.forall{case (scope, cv) => scope contains cv})
  require(splits.flatMap(_._1).subsetOf(problem.variables))
  require(splits.toSeq.map(_._2).size == splits.map(_._2).size, "duplicate conditioning variables")
  
  def allVariableAssignments(vs: Set[Int]): Set[Map[Int,Int]] =
    new DomainCPI(vs.toArray.map(allVariableAssignments).map(_.toArray)).map(_.foldLeft(Map[Int,Int]())(_ ++ _)).toSet
  
  def allVariableAssignments(v: Int): Set[Map[Int,Int]] = 
    (0 until problem.domains(v)).map(value => Map(v -> value))(collection.breakOut)
  
  def conditionsOf(scope: Set[Int]): Set[Map[Int,Int]] = 
    allVariableAssignments(splits.filterNot(_._1.intersect(scope).isEmpty).map(_._2))

  def conditionsOf(variable: Int): Set[Map[Int,Int]] =
    allVariableAssignments(splits.filter(_._1.contains(variable)).map(_._2))

  def compatibleNeighbourConditions(v: Int, cond: Map[Int,Int], neigh: Int): Set[Map[Int,Int]] =
    conditionsOf(neigh).filterNot(nc => contradiction(cond,nc))

  def contradiction(m1: Map[Int,Int], m2: Map[Int,Int]): Boolean =
    (m1.keySet intersect m2.keySet).exists(k => m1(k) != m2(k))

  def toDot: String = {
    def varname(v: Int): String = f"v$v"
    def varlabel(v: Int): String = f"$v"
    def condPort(c: Map[Int,Int]): String =
      "c" + c.toSeq.sortBy(_._1).map{case (vr,vl) => f"${vr}x$vl"}.mkString("O")
    def condLabel(c: Map[Int,Int]): String = condPort(c)

    case class Node(variable: Int, conditions: Set[Map[Int,Int]]){
      def dot: String = {
        def buildConditionPort(c: Map[Int,Int]): String = if(c.isEmpty) "" else
          f"""<TR><TD PORT="${condPort(c)}">${condLabel(c)}</TD></TR>"""
        val cDots = conditions.map(buildConditionPort).mkString("")
        f"""${varname(variable)} [label=<<TABLE BORDER="1" CELLBORDER="0"><TR><TD>${varlabel(variable)}</TD></TR>$cDots</TABLE>>];"""
      }
    }
    case class Edge(src: Int, srcCond: Map[Int,Int], dest: Int, destCond: Map[Int,Int]){
      def dot: String = {
        def buildPort(c: Map[Int,Int]): String = if(c.isEmpty) "" else ":" + condPort(c)
        f"""${varname(src)}${buildPort(srcCond)} -- ${varname(dest)}${buildPort(destCond)}[len="2"];"""
      }
    }

    val nodes: Set[Node] = problem.variables.map(v => Node(v,conditionsOf(v)))

    val edges: Set[Edge] = for{
      Node(dst,dstConds) <- nodes
      dstCond <- dstConds
      src <- problem.neighboursOf(dst) if src > dst
      srcCond <- compatibleNeighbourConditions(dst,dstCond,src)
    } yield Edge(src,srcCond,dst,dstCond)

    val nodeString: String = nodes.map(_.dot).mkString("\n")
    val edgeString: String = edges.map(_.dot).mkString("\n")
    f"""graph G {
        |node [shape=plaintext]
        |$nodeString
        |
        |$edgeString
        |}
    """.stripMargin
  }
}

object Test extends App {
  import vultura.fastfactors.generators._
  val p = grid(3,3,2,expGauss(1),new Random(0))
  val scheme = SimpleScheme(p,Set(Set(0,1,3,4,6,7) -> 3,Set(1,2,4,5,7,8) -> 5))
  println(scheme.toDot)
}
