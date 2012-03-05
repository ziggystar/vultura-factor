package vultura.cnf

import scalaz._
import vultura.factors._

object CNFasBIFun {

  import CNF._

  implicit object ClauseAsFun extends DenseFactor[Clause, BigInt] {
    def variables(f: CNF.Clause): Array[Int] = CNF.variablesOfClause(f)

    def domains(f: CNF.Clause): Array[Array[Int]] = CNF.domainOfClause(f)

    def evaluate(f: CNF.Clause, assignment: Array[Int]): BigInt = {
      val vars = variables(f)
      var vIdx = 0
      var satisfied = false

      while(!satisfied && vIdx < vars.size){
        satisfied |= makesClauseTrue(f,vars(vIdx),assignment(vIdx))
        vIdx += 1
      }
      if(satisfied) 1 else 0
    }

    def condition(f: CNF.Clause, variables: Array[Int], values: Array[Int]): CNF.Clause = {
      //we become true?
      if (variables.zip(values).exists{case (vr,vl) => makesClauseTrue(f,vr,vl)})
        TrueClause
      //False clause is handled correctly, since it is the empty array
      else
        f.filterNot(i => variables.contains(i & ~CNF.NEG_MASK))
    }

    def makesClauseTrue(f: CNF.Clause, variable: Int, value: Int): Boolean =
      if(value == 1) f.contains(variable) else f.contains(variable | CNF.NEG_MASK)
  }
}