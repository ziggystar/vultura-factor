package vultura.inference.gbp

/** Mixin to calculate overcounting numbers for a [[RegionGraph]]. */
trait OvercountingNumbers { self: RegionGraph =>
  protected lazy val overCountingNumbers: Map[Region,Int] = {
    val topological = directedGraph.tarjanSCC.map{
      case ns if ns.size == 1 => ns.head
      case _ => sys.error("region graph graph contains directed cycles")
    }.reverse
    topological.foldLeft(Map[Region,Int]()){case (m,r) => m + (r -> (1 - ancestors(r).foldLeft(0)(_ + m(_))))}
  }
  final override def weightOf(r: Region): Double = overCountingNumbers(r).toDouble
}
