package vultura.fastfactors.algorithms

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.fastfactors.generators._
import vultura.fastfactors.Problem

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class VariableEliminationTest extends Specification {
  override def is: Fragments =
    veSameAsJT(grid(3,3,2,expGauss(1))) ^
    veSameAsJT(grid(1,3,2,expGauss(1))) ^
    veSameAsJT(randomK(10,8,3,2,expGauss(1))) ^
    veSameAsJT(treeK(30,4,2,expGauss(1)))

  def veSameAsJT(p: Problem) = (VariableElimination(p).Z/new JunctionTree(p).Z) must beCloseTo(1,1e-9)
}