package vultura.util.graph

import org.specs2._
import org.specs2.specification.core.SpecStructure

import scala.util.Random

class EdgeMapDiGraphTest extends Specification {
  override def is: SpecStructure =
    s2"dagCover validity for e-r(10,0.5) ${g1DAGCheck(5,0.5)}"

  def g1DAGCheck(n: Int, p: Double) = {
    val g = EdgeMapDiGraph.erdosRenyi(n, p, new Random(0))
    val cover: Set[Set[Int]] = g.computeDAGCover(new Random(0))
    g.isDAGCover(cover) ensuring {
      println(s"graph: $g")
      println(s"covers:\n ${cover.mkString("\n")}")
      true
    }
  }
}
