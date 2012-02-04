package vultura.cnf

/**
 * A CNF representation that uses arrays of integers as clauses. Each variable is an integer and
 * if the high bit is set it's a negated atom.
 *
 * @author Thomas Geier
 * @since 30.01.12
 */
case class CNF(clauses: Array[CNF.Clause])

object CNF {
  /**If highest bit is set, it's a negated atom. */
  type Clause = Array[Int]
  val NEG_MASK: Int = 0x80000000

  val TrueClause: Clause = Array(0, 0 | NEG_MASK)
  val FalseClause: Clause = Array()

  def plainAtoms(clause: Clause) = clause.filter(i => (CNF.NEG_MASK & i) != 0)
  def negatedAtoms(clause: Clause) = clause.filter(i => (CNF.NEG_MASK & i) == 0)

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