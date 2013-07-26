package vultura.factors

import org.specs2._
import specification.Fragments
import scalaz._
import Scalaz._

/**
 * <enter description>
 *
 * @author Thomas Geier
 * @since 01.02.12
 */

class TableFactorTest extends Specification {
  //one variable with two values 0,1 and every entry is 1
  val table1 = TableFactor.fromFunction(
    Array(0),
    Array(0, 1) :: Nil,
    _ => 1
  )
  //two by two table with entries are the sum of arguments
  val table2 = TableFactor.fromFunction(
    Array(0,1),
    Array(0, 1) :: Array(0, 1) :: Nil,
    l => l.sum
  )

  def is: Fragments =
    "create a simple table fun" !
      (table1.evaluate(Array(0)) === 1) ^
      "sum over a variable using genMarg" !
        ((TableFactor.marginalizeDense(table1,Array(0), Array(Array(0, 1)),implicitly[Monoid[Int]]).evaluate(Array()): Int) === 2) ^
      "condition on a variable using genMarg" !
        (TableFactor.marginalizeDense(table1,Array(0), Array(Array(0)),implicitly[Monoid[Int]]).evaluate(Array()) === 1) ^
//doesn't compile anymore, missing implicit, after migration to specs2 2.1
//      "data of table2 must be 0,1,1,2" !
//        (table2.data.deep === Array(0, 1, 1, 2).deep) ^
      "read out table2" !
        ((table2.evaluate(Array(0, 0)) === 0) and
          (table2.evaluate(Array(0, 1)) === 1) and
          (table2.evaluate(Array(1, 0)) === 1) and
          (table2.evaluate(Array(1, 1)) === 2)) ^
  "condition on second value of second variable in table2" !
    (TableFactor.marginalizeDense(table2,Array(1), Array(Array(1)),implicitly[Monoid[Int]]).evaluate(Array(0)) === 1) ^
  "sum over first variable of table2" !
    (TableFactor.marginalizeDense(table2,Array(0), Array(Array(0,1)),implicitly[Monoid[Int]]).evaluate(Array(0)) === 1 and
      TableFactor.marginalizeDense(table2,Array(0), Array(Array(0,1)),implicitly[Monoid[Int]]).evaluate(Array(1)) === 3)
}