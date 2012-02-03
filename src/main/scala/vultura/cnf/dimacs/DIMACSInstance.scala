package vultura.cnf.dimacs

/**
 * Variables are the integers [0,numVariables].
 *
 * User: Thomas Geier
 * Date: 02.05.11 */

case class DIMACSInstance(description: String, numVariables: Int, clauses: Seq[DimacsClause])

case class DimacsClause(plainVariables: Seq[Int], negatedVariables: Seq[Int])
