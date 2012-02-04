package vultura.cnf

import scalaz._
import vultura.factors._

object CNFasBIFun {

  import CNF._

  implicit object ClauseAsFun extends Factor[Clause, BigInt, DenseFactor[BigInt]] {
    def variables(f: CNF.Clause): Array[Int] = f.map(_ & ~CNF.NEG_MASK).distinct.toArray

    def domains(f: CNF.Clause): Array[Array[Int]] = variables(f).map(_ => Array(0,1))

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

    def marginalize(f: CNF.Clause,
                    variables: Array[Int],
                    domains: Array[Array[Int]])(implicit monoid: Monoid[BigInt]): DenseFactor[BigInt] =
      marginalizeDense(f,variables,domains)

    def makesClauseTrue(f: CNF.Clause, variable: Int, value: Int): Boolean =
      if(value == 1) f.contains(variable) else f.contains(variable | CNF.NEG_MASK)
  }
}