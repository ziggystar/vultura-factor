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
class DenseFactor[@specialized T: ClassManifest] protected[DenseFactor](val variables: Array[Int], val domains: Array[Array[Int]], val data: Array[T]) {
  val cpi = new CrossProductIndexer(domains.map(_.size))

  def evaluate(assignment: Array[Int]): T = {
    val domainIndices: Array[Int] = assignment.zip(domains).map(t => t._2.indexOf(t._1))
    val index: Int = cpi.array2Index(domainIndices)
    data(index)
  }

  /**Generalization of marginalization and conditioning. The functions sums over all given domain values
   * for the specified variables. Conditioning means giving only the conditioning value for a certain variable.
   * @param vars These variables will be eliminated from the function.
   * @param doms Sum over those domain values. This array zips with `vars`.
   * @param monoid Responsive for the summing.
   * @return
   */
  def genMarg(_vars: Array[Int], _doms: Array[Array[Int]])(implicit monoid: Monoid[T]): DenseFactor[T] = {
    val (vars, doms) = _vars.zip(_doms).sortBy(_._1).unzip

    val MASK = 0x80000000
    //first look at the new _domains
    val domMap = vars.zip(doms).toMap.withDefaultValue(Array())

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
    val sumOut: Array[Int] => T = {
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
            evaluate(reconstructAssignment(assignment, newAssign))
        }.reduce(_ |+| _)
    }

    DenseFactor.fromFunction(remainingVars, remainingDomains, sumOut)
  }
}


object DenseFactor {
  def fromFunction[T: ClassManifest](_vars: Seq[Int], _domains: Seq[Array[Int]], f: Array[Int] => T) = {
    require(_vars.size == _domains.size, "variable number and domain number don't match")

    val (sortedVars, sortedDomains) = _vars.zip(_domains).sortBy(_._1).unzip

    val cpi = new DomainCPI(sortedDomains)
    val table = new Array[T](cpi.size)
    var i = 0
    cpi.iterator.foreach {
      assign =>
        table(i) = f(assign)
        i += 1
    }

    new DenseFactor(sortedVars.toArray, sortedDomains.map(_.toArray).toArray, table)
  }

  implicit object tfAsFun extends Factor[DenseFactor[BigInt], BigInt] {
    def variables(f: DenseFactor[BigInt]): Array[Int] = f.variables

    def evaluate(f: DenseFactor[BigInt], assignment: Array[Int]): BigInt = null

    def condition(f: DenseFactor[BigInt], variable: Int, value: Int): DenseFactor[BigInt] = null

    def marginalize[C](d: DenseFactor[BigInt], variables: Array[Int], domains: Array[Array[Int]])(implicit monoid: Monoid[BigInt], newFun: Factor[C, BigInt]): C = null
  }

}