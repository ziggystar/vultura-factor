package vultura.cnf

/**Provides implicit conversions to use the DSL to specify CNF objects. */
package object dsl {

  implicit def negatableAtom(s: Symbol) = new {
    def unary_! = DslClause(Nil, s :: Nil)
  }

  implicit def symbolToDslClause(s: Symbol): DslClause = DslClause(s :: Nil, Nil)

  //we need since this implicits don't chain
  implicit def symbolToDslCNF(s: Symbol): DslCNF = DslCNF(DslClause(Seq(s), Nil) :: Nil)
  implicit def dslClauseToDslCNF(c: DslClause): DslCNF = DslCNF(c :: Nil)

  //implicits to Clause
  implicit def symbolToClause(s: Symbol): CNF.Clause = symbolToCNF(s).clauses.head
  implicit def symbolToClause(c: DslClause): CNF.Clause = clauseToCNF(c).clauses.head

  //implicits to CNF
  implicit def symbolToCNF(s: Symbol): CNF = dslCNFToCNF(s)
  implicit def clauseToCNF(c: DslClause): CNF = dslCNFToCNF(c)
  implicit def dslCNFToCNF(f: DslCNF): CNF = CNF.fromSeqTuple(f.clauses.map(cl => (cl.plainAtoms, cl.negatedAtoms)))(math.Ordering.by(_.name))

}