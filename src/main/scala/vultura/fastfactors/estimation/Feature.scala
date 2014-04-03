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

  /** Removes all conditioned variables from the features scope.
    * If the condition contradicts the feature, `None` is returned. */
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

  def toMap: Map[Var,Val] = variables.zip(point).toMap

  override def toString: String =
    s"Feature(VS(${variables.mkString(",")}), Point(${point.mkString(",")}))"
}

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
object Feature {
  /** Features must be in normal domain. */
  def buildProblem(domains: Array[Int], ring: RingZ[Double], weightedFeatures: Iterable[(Feature,Double)]): Problem =
    Problem(weightedFeatures.map{case(f,t) => f.toFastFactor(domains,ring,t)}(collection.breakOut), domains, ring)

  /** Rescales all factors to maximize the number of neutral entries. This will result in a problem that yields a minimal
    * number of features if converted to feature representation.
    * @param p Input problem.
    * @return Equivalent problem with factors rescaled. A constant factor will be added to retain the value of the
    *         partition function.
    */
  def streamLineProblem(p: Problem): Problem = {
    /** rescale a factor and return also the value to multiply into the final correction constant. */
    def rescale(f: FastFactor): (FastFactor, Double) = {
      val mostFrequentValue = f.values.groupBy(identity).maxBy(_._2.size)._1
      val inverse = p.ring.productInverse(mostFrequentValue)
      (f.map(p.ring.prod(inverse,_)), mostFrequentValue)
    }
    val (newFactors,multipliers) = p.factors.map(rescale).unzip
    p.copy(factors = newFactors :+ FastFactor(Array(),Array(p.ring.prodA(multipliers.toArray))))
  }

  def extractFeaturesFromProblem(_p: Problem): Map[Feature,Double] = {
    //rescale factors to yield as many neutral entries as possible to reduce the number of features
    //simplification also guarantees that we can build the map and won't lose duplicate Features (because there ain't any)
    val problem = streamLineProblem(_p).simplify

    (for{
      factor <- problem.factors
      (assignment, value) <- factor.cpi(problem.domains) zip factor.values if value != problem.ring.one
    } yield Feature(factor.variables,assignment) -> value)(collection.breakOut)
  }
}
