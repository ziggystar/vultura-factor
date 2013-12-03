package vultura.cnf.dimacs

import util.Random
import collection.mutable.HashSet
import scala.collection.mutable

object DIMACSGenerator {

  /**
   * Generate a random DimacsClause with the given number of positive and negative atoms.
   *
   * There are no duplicate atoms inside the clause. This also means that the clause is satisfiable on its own.
   */
  def generateClause(numVars: Int, numPosAtoms: Int, numNegAtoms: Int, rnd: Random): DimacsClause = {
    val posAtoms = new mutable.HashSet[Int]
    while (posAtoms.size < numPosAtoms) {
      val candidate = rnd.nextInt(numVars) + 1
      if (!posAtoms.contains(candidate)) posAtoms += candidate
    }

    val negAtoms = new mutable.HashSet[Int]
    while (negAtoms.size < numNegAtoms) {
      val candidate = rnd.nextInt(numVars) + 1
      if (!posAtoms.contains(candidate) && !negAtoms.contains(candidate)) negAtoms += candidate
    }

    DimacsClause(posAtoms.toArray, negAtoms.toArray)
  }

  def generateRandomKSAT(numVariables: Int, clauseSize: Int, clauses: Int, rnd: Random): DIMACSInstance = {
    DIMACSInstance(
      "random generated k-SAT, %d variables, k = %d, %d clauses".format(numVariables, clauseSize, clauses),
      numVariables,
      for (idxCl <- 1 to clauses; posAtoms = rnd.nextInt(clauseSize))
      yield generateClause(numVariables, posAtoms, clauseSize - posAtoms, rnd)
    )
  }
}