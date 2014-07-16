package vultura.cnf

import java.io.File

import vultura.cnf.dimacs.DIMACSParser

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
      val conditionedClause = CNF.conditionClause(clause,trues,falses)
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
  /**If highest bit is set, it's a negated atom. */
  type Clause = Array[Int]
  val NEG_MASK: Int = 0x80000000

  /** Not used publicly. Just use an empty clause. */
  private val FalseClause: Clause = Array()
  val TrueClause: Clause = Array(0, 0 | NEG_MASK)
  val FalseCNF = CNF(IndexedSeq(FalseClause))

  def plainAtoms(clause: Clause) = clause.filter(i => (CNF.NEG_MASK & i) != 0)
  def negatedAtoms(clause: Clause) = clause.filter(i => (CNF.NEG_MASK & i) == 0)

  def variablesOfClause(cnf: Clause): Array[Int] = cnf.map(_ & ~CNF.NEG_MASK).distinct.toArray
  def domainOfClause(clause: Clause): Array[Array[Int]] = variablesOfClause(clause).map(_ => Array(0,1))
  def evaluateClause(clause: Clause, trues: IndexedSeq[Int], falses: IndexedSeq[Int]): Option[Boolean] = {
    val conditioned = conditionClause(clause,trues,falses)
    if(conditioned.isEmpty)
      Some(false)
    else if(conditioned == TrueClause)
      Some(true)
    else
      None
  }

  def variablesOfCNF(cnf: CNF): Array[Int] = cnf.clauses.view.flatMap(variablesOfClause).distinct.toArray
  def domainOfCNF(cnf: CNF): Array[Array[Int]] = variablesOfCNF(cnf).map(_ => Array(0,1))
  def evaluateCNF(cnf: CNF, trues: IndexedSeq[Int], falses:IndexedSeq[Int]): Option[Boolean] = {
    var i = 0
    var stillTrue = true
    var becameFalse = false
    while(i < cnf.clauses.size && !becameFalse){
      evaluateClause(cnf.clauses(i),trues,falses) match {
        case Some(false) => becameFalse = true
        case None => stillTrue = false
        case Some(true) => Unit
      }
      i += 1
    }

    if(becameFalse)
      Some(false)
    else if(stillTrue)
      Some(true)
    else
      None
  }

  def conditionClause(clause: Clause, trues: IndexedSeq[Int], falses: IndexedSeq[Int]): Clause = {
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

  def fromSeqTupleOfInt(clauses: Iterable[(Iterable[Int], Iterable[Int])]): CNF =
    CNF(clauses.map(c => (c._1 ++ c._2.map(_ | NEG_MASK)).toArray).toIndexedSeq)

  def fromFile(cnfFile: String): CNF = DIMACSParser.readFile(cnfFile).toCNF
  def fromFile(cnfFile: File): CNF = DIMACSParser.readFile(cnfFile).toCNF
}