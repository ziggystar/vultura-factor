package vultura.cnf

import scalaz._
import Scalaz._
import vultura.factors.Factor

/**
 * <enter description>
 *
 * @author Thomas Geier
 * Date: 30.01.12
 */

case class CNF(clauses: Array[CNF.Clause])

object CNF {
  /**If highest bit in Int is set, it's a negated atom. */
  type Clause = Array[Int]
  val NEG_MASK: Int = 0x80000000

  val TrueClause: Clause = Array(0, 0 | NEG_MASK)
  val FalseClause: Clause = Array()

  /**
   * Creates a CNF from a list of tuples; each tuple consists of two lists of some ordered type.
   * First tuple entry are the plain atoms and the second one are the negated atoms. To map them to integers,
   * all variables objects are first ordered and then numbered.
   */
  def fromSeqTuple[A: math.Ordering](clauses: Iterable[(Iterable[A], Iterable[A])]): CNF = {
    val allValues: Seq[A] = clauses.flatMap(t => t._1 ++ t._2).toSeq.distinct.sorted
    val atomMap = allValues.zipWithIndex.toMap
    val arrayClauses: Array[CNF.Clause] = clauses
      .map(cl => cl._1.map(atomMap).toArray[Int] ++ cl._2.map(atomMap).map(_ | NEG_MASK).toArray[Int]).toArray
    CNF(arrayClauses)
  }
}

object CNFasBIFun {

  import CNF._

  implicit object CNFasFun extends Factor[CNF, BigInt] {
    def variables(f: CNF): Array[Int] = null

    def evaluate(f: CNF, assignment: Array[Int]): BigInt = null

    def condition(f: CNF, variable: Int, value: Int): CNF = null

    def marginalize[C](d: CNF, variables: Array[Int], domains: Array[Array[Int]])(implicit monoid: Monoid[BigInt], newFun: Factor[C, BigInt]): C = null
  }

  implicit object ClauseAsFun extends Factor[Clause, BigInt] {
    def variables(f: CNF.Clause): Array[Int] =
      f.view.filter(_ != 0).map(math.abs _).distinct.toArray

    def evaluate(f: CNF.Clause, assignment: Array[Int]): BigInt =
      if (f.exists(i => if (i > 0) assignment.contains(i) else !assignment.contains(-i))) 1 else 0

    def condition(f: CNF.Clause, variable: Int, value: Int): CNF.Clause = {
      //we become true?
      if ((value == 1 && f.contains(variable)) || (value == 0 && f.contains(-variable)))
        TrueClause
      //we become false?
      else if (variables(f) == Array(variable))
        FalseClause
      else
        f.filter(i => math.abs(i) != variable)
    }

    def marginalize[C](d: CNF.Clause, variables: Array[Int], domains: Array[Array[Int]])(implicit monoid: Monoid[BigInt], newFun: Factor[C, BigInt]): C = {
      sys.error("implement me")
    }
  }
}