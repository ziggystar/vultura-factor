package vultura.fastfactors.algorithms.gbp

import org.specs2._
import org.specs2.specification.Fragments

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 9/5/13
 */
class RegionGraphTest extends Specification {
  def is: Fragments =
    RegionGraph.setClosure(Set(3,7))(xs => for(x <- xs;y <- xs; z = x - y if z > 0) yield z) === Set(3,7,4,1,6,2,5)
}
