package vultura.cnf.dsl

import scalaz._
import Scalaz._

/**
 * A DSL for writing cnf formulas. Mainly for testing purposes.
 *
 * @author Thomas Geier
 * Date: 31.01.12
 */

case class DslClause(plainAtoms: Seq[Symbol], negatedAtoms: Seq[Symbol]) {
  def v(other: DslClause): DslClause = DslClause(
    (plainAtoms ++ other.plainAtoms).distinct,
    (negatedAtoms ++ other.negatedAtoms).distinct
  )
}

case class DslCNF(clauses: Seq[DslClause]) {
  def &(other: DslCNF): DslCNF = DslCNF(clauses ++ other.clauses)
}

