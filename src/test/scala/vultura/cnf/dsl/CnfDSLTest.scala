package vultura.cnf.dsl

import scala.language.reflectiveCalls
import org.specs2._

/**
 * <enter description>
 *
 * @author Thomas Geier
 * Date: 31.01.12
 */

class CnfDSLTest extends Specification {
  def is =
  //check implicit conversions
    (('x: DslClause) must beAnInstanceOf[DslClause]) ^
      ((!'x: DslClause) must beAnInstanceOf[DslClause]) ^
      (('x: DslCNF) must beAnInstanceOf[DslCNF]) ^
      (('x v 'y) must beAnInstanceOf[DslClause]) ^
      (('x v 'y v !'z) must beAnInstanceOf[DslClause]) ^
      ((('x v !'y) & 'x) === DslCNF(DslClause('x :: Nil, 'y :: Nil) :: DslClause('x :: Nil, Nil) :: Nil))
}