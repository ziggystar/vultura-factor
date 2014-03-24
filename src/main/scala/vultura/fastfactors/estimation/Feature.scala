package vultura.fastfactors.estimation

import vultura.fastfactors._
import scala.util.hashing.MurmurHash3

case class Feature(variables: Array[Var], point: Array[Val]) {
  /**
   * @param domains Maps variables to their domain size.
   * @param ring Ring used by resulting factor.
   * @param theta Must be in normal domain.
   * @return A FastFactor yielding `theta` for assignments in `points`, and one otherwise.
   */
  def toFastFactor(domains: Array[Int], ring: RingZ[Double], theta: Double) =
    FastFactor(variables,Array.fill(variables.map(domains).product)(ring.one)).set(point,domains,theta)

  /** Removes all conditioned variables from the features scope and also removes all contradicted points. */
  def condition(c: Map[Var,Val]): Option[Feature] = {
    val compatibleVals = variables.zip(point).count{ case (variable, value) => c.get(variable).forall(_ == value) }
    if(compatibleVals == point.size){
      val (newVars, newVals) = variables.zip(point).filterNot(ass => c.contains(ass._1)).unzip
      Some(Feature(newVars.toArray,newVals.toArray))
    }
    else
      None
  }

  lazy val memoHash = (MurmurHash3.arrayHash(variables), MurmurHash3.arrayHash(point)).hashCode
  override def hashCode(): Int = memoHash

  override def equals(obj: scala.Any): Boolean = obj match {
    case f@Feature(v,p) =>
      f.hashCode() == this.hashCode &&
        f.variables.deep == this.variables.deep &&
        f.point.deep == this.point.deep
    case _ => false
  }

  override def toString: String =
    s"Feature(VS(${variables.mkString(",")}), Points(${point.mkString(",")}))"
}

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
object Feature {
  /** Features must be in normal domain. */
  def buildProblem(domains: Array[Int], ring: RingZ[Double], weightedFeatures: Iterable[(Feature,Double)]): Problem =
    Problem(weightedFeatures.map{case(f,t) => f.toFastFactor(domains,ring,t)}(collection.breakOut), domains, ring)
}
