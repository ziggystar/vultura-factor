package vultura.inference.sampling

import collection.mutable.WrappedArray
import vultura.factors.{Factor, SparseFactor}
import util.Random
import vultura.util.Measure
import scalaz._
import Scalaz._

class ParticleSeq(val variables: Array[Int],
                  val domains: Array[Array[Int]],
                  _particles: Seq[(WrappedArray[Int], Double)],
                  measure: Measure[Double],
                  sumMonoid: Monoid[Double]){
  val particles: IndexedSeq[(WrappedArray[Int], Double)] = _particles.groupBy(_._1).mapValues(_.map(_._2).reduce(sumMonoid.append(_,_))).toIndexedSeq
  val partition = particles.map(p => p._2).foldLeft(sumMonoid.zero)(sumMonoid.append(_,_))
  assert(variables.distinct.size == variables.size, "double entries in `variables`")
  def drawParticle(random: Random): Option[WrappedArray[Int]] =
    vultura.util.drawRandomlyByIS(particles,random)(t => measure.normalizedWeight(partition)(t._2)).map(_._1)
  def generator(random: Random): Iterator[WrappedArray[Int]] = Iterator.continually(drawParticle(random)).flatten
}

object ParticleSeq {
  implicit def particleSetAsSparseFactor(m: Measure[Double], sum: Monoid[Double]): SparseFactor[ParticleSeq,Double] = new SparseFactor[ParticleSeq,Double] {
    def variables(f: ParticleSeq): Array[Int] = f.variables
    def domains(f: ParticleSeq): Array[Array[Int]] = f.domains
    def condition(f: ParticleSeq, variables: Array[Int], values: Array[Int]): ParticleSeq = {
      val (condVariables,condDomains) = (f.variables,f.domains).zipped.filterNot(t => variables.contains(t._1)).unzip
      //maps indices into f.variables to their demanded values
      val conditionMap: Array[(Int, Int)] = variables.map(f.variables.indexOf(_)).zip(values)
      val survivingIndices: Array[Int] = f.variables.zipWithIndex.filterNot(t => variables.contains(t._1)).map(_._2)
      def matchCondition(wa: WrappedArray[Int]): Boolean = conditionMap.forall{case (idx,v) => wa(idx) == v}
      val condParticles = f.particles.collect{
        case (wrappedArray,value) if(matchCondition(wrappedArray)) =>
          (wrapIntArray(survivingIndices.map(wrappedArray)),value)
      }
      new ParticleSeq(condVariables.toArray,condDomains.toArray,condParticles,m,sum)
    }

    def defaultValue(f: ParticleSeq): Double = 0d

    def points(f: ParticleSeq): Map[WrappedArray[Int], Double] = f.particles.toMap
  }

  def apply[A](factor: A, particles: Seq[(Array[Int],Double)],m: Measure[Double], sum: Monoid[Double])(implicit evF: Factor[A,Double]): ParticleSeq = {
    import scalaz._
    import Scalaz._
    new ParticleSeq(
      evF.variables(factor),
      evF.domains(factor),
      particles map ((wrapIntArray _) <-: _),
      m,
      sum
    )
  }

  def unapply(ps: ParticleSeq): Some[(Array[Int], Array[Array[Int]], Seq[(WrappedArray[Int], Double)])] =
    Some(ps.variables, ps.domains, ps.particles)
}

