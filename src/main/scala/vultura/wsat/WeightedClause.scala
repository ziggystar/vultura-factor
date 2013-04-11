package vultura.wsat

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 3/15/13
 */
case class WeightedClause(literals: Array[Literal], weight: Double){
  def eval(assignment: Map[Int,Int]): Double = if (isSatisfied(assignment)) weight else 1d
  def eval(assignment: Array[Int]): Double = if(isSatisfied(assignment)) weight else 1d

  def isSatisfied(assignment: Array[Int]): Boolean = {
    var liti = 0
    while(liti < literals.size){
      val lit: Literal = literals(liti)
      if(assignment(lit.variable) == lit.sign)
        return true
      liti += 1
    }
    false
  }
  def isSatisfied(assignment: Map[Int, Int]): Boolean =
    literals.exists(l => assignment(l.variable) == l.sign)

  override def toString: String = "(%s,%g)".format(literals.mkString("v"),weight)
}

/** Encodes literals. The sign is stored in the LSB. One is positive, 0 is negative.*/
final class Literal(val data: Int) extends AnyVal {
  @inline def isPositive: Boolean = sign == 0
  @inline def sign: Int = data & 1
  @inline def variable: Int = data >> 1
  override def toString: String = (if (isPositive) "" else "-") + variable
}

object Literal{
  def apply(variable: Int, sign: Boolean) = new Literal((variable << 1) + (if(sign) 1 else 0))

  /**
   * @param variable
   * @param sign 1 for positive variable, 0 for negative variable.
   * @return
   */
  def apply(variable: Int, sign: Int) = {
    require(variable >= 0)
    require(sign == 0 || sign == 1)
    new Literal((variable << 1) + sign)
  }
}
