package vultura.factors

import scalaz._
import Scalaz._
import vultura.util._

/**
 * Stores all function values inside an array.
 * `variables` is guaranteed to be sorted.
 *
 * @param variables
 * @param domains
 * @param data
 */
class DenseFactor[@specialized T: ClassManifest] protected[DenseFactor](val variables: Array[Int],
                                                                        val domains: Array[Array[Int]],
                                                                        val data: Array[T])
extends SelfFactor[T]{
  val cpi = new CrossProductIndexer(domains.map(_.size))

  def evaluate(assignment: Array[Int]): T = {
    val domainIndices: Array[Int] = assignment.zip(domains).map(t => t._2.indexOf(t._1))
    val index: Int = cpi.array2Index(domainIndices)
    data(index)
  }

  /** Condition via marginalization. */
  def condition(vars: Array[Int],
                values: Array[Int]): SelfFactor[T] = new FactorView[SelfFactor[T],T](vars.zip(values).toMap, this)
  
}

class FactorView[A,R](val condition: Map[Int,Int], val factor: A)(implicit evF: Factor[A,R]) extends SelfFactor[R] {
  import vultura.{factors => vf}
  val variables = vf.variables(factor).filterNot(condition.contains)
  val domains = variables.map(vf.variables(factor).zip(vf.domains(factor)).toMap)
  def evaluate(assignment: Array[Int]) = vf.evaluate(factor,variables.map((variables zip assignment).toMap ++ condition))
  def condition(variables: Array[Int], values: Array[Int]) = new FactorView(condition ++ (variables zip values).toMap,factor)(evF)
}


object DenseFactor {
  def fromFunction[T: ClassManifest](_vars: Seq[Int], _domains: Seq[Array[Int]], f: Array[Int] => T) = {
    require(_vars.size == _domains.size, "variable number and domain number don't match")

    val (sortedVars, sortedDomains) = _vars.zip(_domains).sortBy(_._1).unzip

    val cpi = new DomainCPI(seqarray2aa(sortedDomains))
    val table = new Array[T](cpi.size)
    var i = 0
    cpi.iterator.foreach {
      assign =>
        table(i) = f(assign)
        i += 1
    }

    new DenseFactor(sortedVars.toArray, sortedDomains.map(_.toArray).toArray, table)
  }

  /**
   * Generalization of marginalization and conditioning. This method sums over all given domain values
   * for the specified variables. Specifying only one domain value for a variable is equivalent to conditioning
   * on this value.
   * Note: The function must not rely on `monoid` being non-null if no domain contains more than one entry.
   *
   * @param f Object to apply to.
   * @param _vars These variables will be eliminated from the function.
   * @param _doms Zips with `variables`. Contains at least one domain value for each variable in `variables`. If multiple values are given for
   *  a certain variable, the variable is eliminated by summing over the given values.
   * @param monoid Use this for summing the result values of type `T`.
   * @return A `Factor` that does not depend on any variable in `variables`.
   */
  def marginalizeDense[A,B](f: A,
                            _vars: Array[Int],
                            _doms: Array[Array[Int]])
                           (implicit evF: Factor[A,B],
                            monoid: Monoid[B],
                            manifestB: ClassManifest[B]): DenseFactor[B] = {
    import vultura.{factors => vf}

    val (vars, doms) = _vars.zip(_doms).sortBy(_._1).unzip

    val MASK = 0x80000000

    val (variables,domains) = (vf.variables(f),vf.domains(f))

    val (remainingVars, remainingDomains) = variables.zip(domains).filterNot(t => vars.contains(t._1)).unzip

    //this tells us where to look for a value when constructing an assignment to this Fun
    //if high bit is unset remainingVars and else to the summed out vars
    //this is not in imperative style for optimization purposes but for readability:/
    val lookUp: Array[Int] = {
      var intoVars = 0
      var intoRemaining = 0
      var intoVariables = 0
      val result = new Array[Int](variables.size)
      while (intoVariables < variables.size) {
        if (intoVars < vars.size && variables(intoVariables) == vars(intoVars)) {
          result(intoVariables) = intoVars | MASK
          intoVars += 1
        } else {
          assert(variables(intoVariables) == remainingVars(intoRemaining))
          result(intoVariables) = intoRemaining
          intoRemaining += 1
        }
        intoVariables += 1
      }

      result
    }

    val _cpi = new DomainCPI(doms)
    val reusedAssignment = new Array[Int](variables.size)

    //fix the assignment to the remaining variables and sum over everything in vars/doms
    //the argument to sumOut goes to the remaining variables
    val sumOut: Array[Int] => B = {
      assignment =>
        def reconstructAssignment(remainAss: Array[Int], newAss: Array[Int]): Array[Int] = {
          var i = 0
          while (i < reusedAssignment.size) {
            val index = lookUp(i)
            val value = if ((index & MASK) != 0) newAss(index ^ MASK) else remainAss(index)
            reusedAssignment(i) = value
            i += 1
          }
          reusedAssignment
        }

        _cpi.iterator.map {
          newAssign =>
            evaluate(f,reconstructAssignment(assignment, newAssign))
        }.reduce(_ |+| _)
    }

    DenseFactor.fromFunction(remainingVars, remainingDomains, sumOut)
  }
}