package vultura.fastfactors.algorithms.conditioned

import org.specs2.Specification
import org.specs2.specification.Fragments

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class LSchemeTest extends Specification {
  def emptyScheme = DCon()
  def twoIndep = DCon(DDis(0,Map(0 -> DCon(),1 -> DCon())), DDis(1,Map(0->DCon(),1->DCon())))

  override def is: Fragments =
    (emptyScheme.usedVariables === Set()) ^
    (twoIndep.usedVariables === Set(0,1)) ^
    (twoIndep.partialAssignments.toSet === (for(x <- 0 to 1; y <- 0 to 1) yield Map(0 -> x,1 -> y)).toSet) ^
    (emptyScheme.partialAssignments.toSet === Set(Map()))


}
