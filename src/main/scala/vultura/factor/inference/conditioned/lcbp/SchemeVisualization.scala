package vultura.factor.inference.conditioned.lcbp

import vultura.factor.inference.conditioned.Condition

import scala.collection.immutable.IndexedSeq

/** Creates graphviz and tikz visualizations for conditioning schemes. */
object SchemeVisualization {
  /** Check whether the given conditions do not contradict each other. */
  def conditionsCompatible(c1: Condition, c2: Condition): Boolean =
    c1.keySet.intersect(c2.keySet).forall(v => c1(v) == c2(v))

  implicit def seqOrdering[X](implicit ox: Ordering[X]): Ordering[Seq[X]] = Ordering.fromLessThan[Seq[X]]{ case (sx1,sx2) =>
    sx1.size < sx2.size || (sx1.size == sx2.size && sx1.zip(sx2).dropWhile(x => ox.equiv(x._1, x._2)).headOption.map(x => ox.lt(x._1, x._2)).getOrElse(true))
  }

  implicit val conditionOrdering: Ordering[Condition] = Ordering.by((c: Condition) => c.keys.toSeq.sorted.map(c))

  def wrapWithlatexTemplate(body: String): String =
    s"""\\documentclass{standalone}
       |\\usepackage{tikz}
       |\\usetikzlibrary{,backgrounds}
       |\\begin{document}
       |$body
       |\\end{document}
      """.stripMargin

  /** Plot the Markov network with tikz, you need the 3d package. */
  def tikz3DMarkovNetwork(scheme: FactoredScheme, graphLayout: Int => (Double,Double), nodeLabels: Map[Int,String] = Map()): String = {

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
      (x, y, z) = nodePosition(v, condition)
      nodeId = vID(v, condition)
      nodeLabel: String = nodeLabels.getOrElse(v, "")
    } yield s"\\node[V] ($nodeId) at ($x,$y,$z) {$nodeLabel};"

    val markovEdges: IndexedSeq[Set[Int]] = for {
      v <- scheme.problem.variables
      n <- scheme.problem.neighboursOfVariableEx(v)
    } yield Set(v, n)

    val conditionedEdges: Seq[((Int, Condition), (Int, Condition))] = (for {
      Seq(n1, n2) <- markovEdges.map(_.toSeq)
      c1 <- scheme.conditionsOf(Set(n1))
      c2 <- scheme.conditionsOf(Set(n2)) if conditionsCompatible(c1, c2)
    } yield ((n1, c1), (n2, c2)))(collection.breakOut)

    val edgeStrings: Seq[String] = for {
      ((v1, c1), (v2, c2)) <- conditionedEdges
    } yield s"\\draw[E] (${vID(v1, c1)}) edge (${vID(v2, c2)});"

    s"""\\begin{tikzpicture}[
       |  z={(-0.6,0.3)},
       |  x={(0.6,0.3)},
       |  y={(0,0.3)},
       |  V/.style={circle,draw,fill=white,inner sep=1},
       |  E/.style={}
       |]
       |${nodeStrings.mkString("\n")}
       |
       |\\begin{pgfonlayer}{backgrounds}
       |${edgeStrings.mkString("\n")}
       |\\end{pgfonlayer}
       |\\end{tikzpicture}
     """.stripMargin
  }
}

object ConditionedGridGraphs {
  import SchemeVisualization._

  def main(args: Array[String]) {
    val width = 5
    val height = 5
    val dist = 1

    def getL0Neighbours(d: Int, x: Int, y: Int): Set[(Int,Int)] = (for{
      xx <- x-d to x+d if xx >= 0 && xx < width
      yy <- y-d to y+d if yy >= 0 && yy < height
    } yield (xx,yy))(collection.breakOut)

    def coord2var(xy: (Int,Int)) = xy._1 + xy._2 * width
    def var2coord(v: Int): (Int,Int) = (v % width, v / width)
    def tuple2double(ti: (Int,Int)) = (ti._1.toDouble,ti._2.toDouble)

    val conditioners = Set((0,0),(2,0))
    val problem = vultura.factor.generators.grid(width, height)
    val scheme = FactoredScheme.fromInfluenceMap(
      problem,
      conditioners.map(c => coord2var(c) -> getL0Neighbours(dist,c._1,c._2).map(coord2var))(collection.breakOut))

    println(wrapWithlatexTemplate(tikz3DMarkovNetwork(scheme, tuple2double _ compose var2coord)))
  }
}


