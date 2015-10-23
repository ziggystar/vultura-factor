package vultura.factor.generation

import org.specs2.Specification
import vultura.factor.generation.graph._

class GraphGeneratorTest extends Specification {

  def is =
    s2"""Check constant degree of some graphs
        |
        |  torodial 2d lattice has only nodes of degree 4 ${allDegreesAre(_ == 4)(lattice(3 -> true, 3 -> true))}
        |  torodial 3d lattice has only nodes of degree 6 ${allDegreesAre(_ == 6)(lattice(3 -> true, 3 -> true, 3 -> true))}
        |
        |  complete graph 6 has only nodes of degree 5 ${allDegreesAre(_ == 5)(complete(6))}
        |  empty graph 6 has only nodes of degree 0 ${allDegreesAre(_ == 0)(unconnected(6))}
        |
        |  2d lattice with one wrapping dim has degrees of 3 and 4 ${allDegreesAre(Set(3,4))(lattice(3 -> true, 3 -> true))}
        |
        |  2d grid 3x4 has 12 nodes ${lattice(3 -> false, 4 -> false).nodes.size === 12}
        |""".stripMargin

  def degreesOf[N](g: Graph[N]): Seq[Int] = g.nodes.toSeq.map(g.neighboursOf).map(_.size)
  def allDegreesAre[N](cond: Int => Boolean)(g: Graph[N]) = forall(degreesOf(g))(cond)
}
