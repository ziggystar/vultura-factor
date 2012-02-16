package vultura.cnf.dimacs

import vultura.cnf.CNF

/**
 * Variables are the integers [0,numVariables].
 *
 * User: Thomas Geier
 * Date: 02.05.11 */

case class DIMACSInstance(description: String, numVariables: Int, clauses: Seq[DimacsClause]) {
  def toCNF: CNF = CNF.fromSeqTuple(clauses.map(dc => (dc.plainVariables,dc.negatedVariables)))
}

case class DimacsClause(plainVariables: Seq[Int], negatedVariables: Seq[Int])