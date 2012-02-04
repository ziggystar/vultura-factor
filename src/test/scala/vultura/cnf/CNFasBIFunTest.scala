package vultura.cnf

import org.specs2._
import specification.Fragments

import dsl._
import CNFasBIFun._
import vultura.factors._

/**
 * @author Thomas Geier
 * @since 03.02.12
 */

class CNFasBIFunTest extends Specification {
  def is: Fragments =
  {variables('x: CNF.Clause) ===(Array(0))} ^
  {domains('x: CNF.Clause).deep === Array(Array(0,1)).deep} ^
  (variables('x v !'y: CNF.Clause).deep === Array(0,1).deep) ^
  (domains('x v !'y: CNF.Clause).deep === Array(Array(0,1),Array(0,1)).deep) ^
    {
      val conditioned: CNF.Clause = condition('x v 'y: CNF.Clause, Array(0), Array(0))
      conditioned.deep === Array(1).deep
    } ^
  (condition('x v 'y v 'z: CNF.Clause, Array(0), Array(1)) === CNF.TrueClause)
}