package vultura.inference.sampling

import collection.mutable.WrappedArray
import vultura.factors.{Factor, SparseFactor}
import util.Random
import vultura.util.Measure
import scalaz._
import Scalaz._

/** Pparicles are sorted by weight. */
class ParticleSeq(val variables: Array[Int],
                  val domains: Array[Array[Int]],
                  _particles: Seq[(WrappedArray[Int], Double)],
                  val measure: Measure[Double]){
  val sumMonoid = measure.sum
  val particles: IndexedSeq[(WrappedArray[Int], Double)] = {
    val grouped: IndexedSeq[(WrappedArray[Int], Double)] = _particles.groupBy(_._1).mapValues(_.map(_._2).reduce(sumMonoid.append(_,_))).toIndexedSeq
    grouped.sortBy(_._2).reverse
  }
  lazy val partition = particles.map(_._2).foldLeft(sumMonoid.zero)(sumMonoid.append(_,_))

  assert(variables.distinct.size == variables.size, "double entries in `variables`")
  assert(particles.forall(_._1.size == variables.size), "too short particles")

  //todo refactor this; put product into Measure
  /** Multiplies a factor to each particle. */
  def multiplyWeight(f: WrappedArray[Int] => Double, product: Monoid[Double]): ParticleSeq =
    new ParticleSeq(variables, domains, particles.map(p => p._1 -> product.append(p._2,f(p._1))), measure)

  def drawParticle(random: Random): Option[WrappedArray[Int]] =
    if(measure.isPositive(partition))
        Some(vultura.util.drawRandomlyByIS(particles,random)(t => measure.normalizedWeight(partition)(t._2)).map(_._1).get)
      else
        None
  def generate(random: Random, num: Int): Option[IndexedSeq[WrappedArray[Int]]] = if(measure.isPositive(partition))
    Some(IndexedSeq.fill(num)(drawParticle(random).get))
  else
    None

  def normalized: ParticleSeq = {
    val normalizedSeq = particles.map(_._1).zip(measure.normalize(particles.map(_._2).toArray))
    new ParticleSeq(variables, domains, normalizedSeq, measure)
  }
}

object ParticleSeq {
  implicit val particleSetAsSparseFactor: SparseFactor[ParticleSeq,Double] = new SparseFactor[ParticleSeq,Double] {
    def variables(f: ParticleSeq): Array[Int] = f.variables
    def domains(f: ParticleSeq): Array[Array[Int]] = f.domains
    def condition(f: ParticleSeq, variables: Array[Int], values: Array[Int]): ParticleSeq = {
      //the remaining variables and their domains
      val (condVariables,condDomains) = (f.variables,f.domains).zipped.filterNot(t => variables.contains(t._1)).unzip

      //maps indices into f.variables to their demanded values; this is the conditioned part
      val conditionMap: Array[(Int, Int)] = variables.map(f.variables.indexOf(_)).zip(values)

      //map indices of variables before conditioning to their index after conditioning
      val survivingIndices: Array[Int] = f.variables.zipWithIndex.filterNot(t => variables.contains(t._1)).map(_._2)

      def matchCondition(wa: WrappedArray[Int]): Boolean = conditionMap.forall{case (idx,v) => wa(idx) == v}
      val condParticles = f.particles.collect{
        case (wrappedArray,value) if(matchCondition(wrappedArray)) =>
          (wrapIntArray(survivingIndices.map(wrappedArray)),value)
      }
      new ParticleSeq(condVariables.toArray,condDomains.toArray,condParticles,f.measure)
    }

    def defaultValue(f: ParticleSeq): Double = 0d

    def points(f: ParticleSeq): Map[WrappedArray[Int], Double] = f.particles.toMap
  }

  def apply[A](factor: A, particles: Seq[(Array[Int],Double)],m: Measure[Double])(implicit evF: Factor[A,Double]): ParticleSeq = {
    import scalaz._
    import Scalaz._
    new ParticleSeq(
      evF.variables(factor),
      evF.domains(factor),
      particles map ((wrapIntArray _) <-: _),
      m
    )
  }

  def unapply(ps: ParticleSeq): Some[(Array[Int], Array[Array[Int]], Seq[(WrappedArray[Int], Double)])] =
    Some(ps.variables, ps.domains, ps.particles)
}

