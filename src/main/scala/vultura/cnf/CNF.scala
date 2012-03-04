package vultura.cnf

/**
 * A CNF representation that uses arrays of integers as clauses. Each variable is an integer and
 * if the high bit is set it's a negated atom.
 *
 * @author Thomas Geier
 * @since 30.01.12
 */
case class CNF(clauses: IndexedSeq[CNF.Clause]){
  def condition(trues: IndexedSeq[Int], falses: IndexedSeq[Int]): CNF = {
    var formulaFalse = false
    var clauseIdx = 0
    val builder = IndexedSeq.newBuilder[CNF.Clause]
    while(clauseIdx < clauses.size && !formulaFalse){
      val clause = clauses(clauseIdx)
      val conditionedClause = CNF.condition(clause,trues,falses)
      if(conditionedClause.isEmpty)
        formulaFalse = true
      else if(conditionedClause != CNF.TrueClause)
        builder += conditionedClause
      clauseIdx += 1
    }
    if(formulaFalse)
      CNF.FalseCNF
    else
      CNF(builder.result())
  }

}

object CNF {
  val FalseCNF = CNF(IndexedSeq(FalseClause))
  /**If highest bit is set, it's a negated atom. */
  type Clause = Array[Int]
  val NEG_MASK: Int = 0x80000000

  val TrueClause: Clause = Array(0, 0 | NEG_MASK)
  val FalseClause: Clause = Array()

  def plainAtoms(clause: Clause) = clause.filter(i => (CNF.NEG_MASK & i) != 0)
  def negatedAtoms(clause: Clause) = clause.filter(i => (CNF.NEG_MASK & i) == 0)

  def condition(clause: Clause, trues: IndexedSeq[Int], falses: IndexedSeq[Int]): Clause = {
    val builder = Array.newBuilder[Int]
    var i = 0
    var evalTrue = false
    while(i <= clause.size && !evalTrue){
      val literal = clause(i)
      var literalFalse = false
      def isNegated: Boolean = (literal & CNF.NEG_MASK) != 0
      if(isNegated){
        val variable = literal & ~CNF.NEG_MASK
        if(falses.contains(variable))
          evalTrue = true
        if(trues.contains(variable))
          literalFalse = true
      } else {
        val variable = literal
        if(trues.contains(variable))
          evalTrue = true
        if(falses.contains(variable))
          literalFalse = true
      }
      if(!(literalFalse || evalTrue))
        builder += literal
      i += 1
    }
    if(evalTrue)
      TrueClause
    else
      builder.result()
  }

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