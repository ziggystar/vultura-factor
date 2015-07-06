package vultura.factor.inference.conditioned.lcbp

import java.io.{PrintStream, FileOutputStream}

import vultura.factor.Problem
import vultura.factor.inference.conditioned.Condition

import scala.collection.immutable.IndexedSeq

/** Creates graphviz and tikz visualizations for conditioning schemes. */
object SchemeVisualization {

  implicit def seqOrdering[X](implicit ox: Ordering[X]): Ordering[Seq[X]] = Ordering.fromLessThan[Seq[X]]{ case (sx1,sx2) =>
    sx1.size < sx2.size || (sx1.size == sx2.size && sx1.zip(sx2).dropWhile(x => ox.equiv(x._1, x._2)).headOption.map(x => ox.lt(x._1, x._2)).getOrElse(true))
  }

  implicit val conditionOrdering: Ordering[Condition] = Ordering.by((c: Condition) => c.keys.toSeq.sorted.map(c))

  def wrapWithlatexTemplate(body: String): String =
    s"""\\documentclass{standalone}
       |\\usepackage{tikz}
       |\\usetikzlibrary{3d,backgrounds}
       |
       |\\tikzset{iso graph/.style={
       |  z={(-0.6,0.3)},
       |  x={(0.6,0.3)},
       |  y={(0,0.3)},
       |  V/.style={circle,draw,fill=white,inner sep=1},
       |  E/.style={},
       |  conditioned node/.style={fill=black!15},
       |  conditioned edge/.style={opacity=0.15},
       |}}
       |
       |\\begin{document}
       |$body
       |\\end{document}
      """.stripMargin

  /** Plot the Markov network with tikz, you need the 3d package. */
  def tikz3DMarkovNetwork(scheme: Scheme, graphLayout: Int => (Double,Double), nodeLabels: Map[Int,String] = Map()): String = {

    def indexOfCondition(v: Int, c: Condition): Int = scheme.conditionsOf(Set(v)).toSeq.sorted.indexOf(c)

    def vID(v: Int, c: Condition): String = s"node $v ${indexOfCondition(v, c)}"

    def nodePosition(v: Int, c: Condition): (Double, Double, Double) = {
      val (x, y) = graphLayout(v)
      val z = indexOfCondition(v, c) - (scheme.conditionsOf(Set(v)).size - 1) / 2d
      (x,z,y)
    }

    val nodeStrings: Seq[String] = for {
      v <- scheme.problem.variables
      condition <- scheme.conditionsOf(Set(v))
      vStyle = (Seq("V") ++ (if(condition.contains(v)) Some("conditioned node") else None)).mkString(",")
      (x, y, z) = nodePosition(v, condition)
      nodeId = vID(v, condition)
      nodeLabel: String = nodeLabels.getOrElse(v, "")
    } yield s"\\node[$vStyle] ($nodeId) at ($x,$y,$z) {$nodeLabel};"

    val markovEdges: IndexedSeq[Set[Int]] = for {
      v <- scheme.problem.variables
      n <- scheme.problem.neighboursOfVariableEx(v)
    } yield Set(v, n)

    val conditionedEdges: Seq[((Int, Condition), (Int, Condition))] = (for {
      Seq(n1, n2) <- markovEdges.map(_.toSeq)
      c1 <- scheme.conditionsOf(Set(n1))
      c2 <- scheme.conditionsOf(Set(n2)) if scheme.conditionsCompatible(c1, c2)
    } yield ((n1, c1), (n2, c2)))(collection.breakOut)

    val edgeStrings: Seq[String] = for {
      ((v1, c1), (v2, c2)) <- conditionedEdges
      edgeStyle = (Seq("E") ++ (if(c1.keys.exists(Set(v1,v2))) Seq("conditioned edge") else Seq())).mkString(",")
    } yield s"\\draw[$edgeStyle] (${vID(v1, c1)}) edge (${vID(v2, c2)});"

    s"""\\begin{tikzpicture}[iso graph]
       |${nodeStrings.mkString("\n")}
       |
       |\\begin{pgfonlayer}{background}
       |${edgeStrings.mkString("\n")}
       |\\end{pgfonlayer}
       |\\end{tikzpicture}
     """.stripMargin
  }
}

object ConditionedGridGraphs {
  import SchemeVisualization._


  def flGrid(width: Int, height: Int, dist: Int = 1, conditioners: Set[(Int, Int)] = Set()): (Scheme,Int => (Double,Double)) = {

    def getL0Neighbours(d: Int, x: Int, y: Int): Set[(Int,Int)] = (for{
      xx <- x-d to x+d if xx >= 0 && xx < width
      yy <- y-d to y+d if yy >= 0 && yy < height
    } yield (xx,yy))(collection.breakOut)

    def coord2var(xy: (Int,Int)) = xy._1 + xy._2 * width
    def var2coord(v: Int): (Int,Int) = (v % width, v / width)
    def tuple2double(ti: (Int,Int)) = (ti._1.toDouble,ti._2.toDouble)

    val problem = vultura.factor.generators.grid(width, height)
    val scheme = FactoredScheme.fromInfluenceMap(
      problem,
      conditioners.map(c => coord2var(c) -> getL0Neighbours(dist,c._1,c._2).map(coord2var))(collection.breakOut))

    (scheme, tuple2double _ compose var2coord)
  }

  def fullyConditionedGrid(width: Int, height: Int, conditions: Set[Condition]): (Scheme,Int => (Double,Double)) = {
    def getL0Neighbours(d: Int, x: Int, y: Int): Set[(Int,Int)] = (for{
      xx <- x-d to x+d if xx >= 0 && xx < width
      yy <- y-d to y+d if yy >= 0 && yy < height
    } yield (xx,yy))(collection.breakOut)

    def coord2var(xy: (Int,Int)) = xy._1 + xy._2 * width
    def var2coord(v: Int): (Int,Int) = (v % width, v / width)
    def tuple2double(ti: (Int,Int)) = (ti._1.toDouble,ti._2.toDouble)

    val scheme = new Scheme{
      val problem: Problem = vultura.factor.generators.grid(width, height)

      /** Compute the condition for `vars` that is super-condition to `c`. */
      def superConditionOf(c: GC, vars: Set[Int]): GC = ???
      def conditionsOf(variables: Set[Int]): Set[GC] = conditions
      def subConditionsOf(c: GC, vars: Set[Int]): Set[GC] = ???
      def allowedValuesUnderCondition(variable: Int, condition: GC): Set[Int] = ???
    }

    (scheme, tuple2double _ compose var2coord)
  }

  def main(args: Array[String]) {
    val jobs = Seq(
      "cbp_A.tex" -> fullyConditionedGrid(4,4,Set(Map())),
      "cbp_B.tex" -> fullyConditionedGrid(4,4,Set(Map(0 -> 0), Map(0 -> 1))),
      "cbp_C.tex" -> fullyConditionedGrid(4,4,Set(Map(0 -> 0, 15 ->0), Map(0 -> 0, 15 -> 1), Map(0 -> 1))),
      "cbp_D.tex" -> fullyConditionedGrid(4,4,Set(Map(0 -> 0, 15 ->0), Map(0 -> 0, 15 -> 1), Map(0 -> 1, 15 -> 0),Map(0 -> 1, 15 -> 1))),
      "lcbp_A.tex" -> flGrid(4, 4, 1, Set()),
      "lcbp_B.tex" -> flGrid(4, 4, 1, Set((0,0))),
      "lcbp_D.tex" -> flGrid(4, 4, 1, Set((0,0),(3,3)))
    )

    jobs foreach { case (fileName, (scheme,layout)) =>
      val outstream = new PrintStream(new FileOutputStream(fileName))
      outstream.println(wrapWithlatexTemplate(tikz3DMarkovNetwork(scheme, layout)))
      outstream.close()
    }
  }
}


