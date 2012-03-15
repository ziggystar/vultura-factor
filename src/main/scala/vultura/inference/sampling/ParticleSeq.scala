package vultura.inference.sampling

import collection.mutable.WrappedArray
import vultura.factors.{Factor, SparseFactor}
import util.Random

case class ParticleSeq(variables: Array[Int],
                       domains: Array[Array[Int]],
                       particles: Map[WrappedArray[Int], Seq[Double]]){
  assert(variables.distinct.size == variables.size, "double entries in `variables`")

  def resample(numParticles: Int, random: Random): ParticleSeq = {
    val flatParticles  = particles.mapValues(_.sum).toSeq
    def drawParticle: Option[WrappedArray[Int]] = vultura.util.drawRandomlyBy(flatParticles,random)(t => t._2).map(_._1)
    val newParticles: Map[WrappedArray[Int], Seq[Double]] =
      Iterator.continually(drawParticle).flatten.take(numParticles).toSeq.groupBy(x => x).mapValues(_.map(_ => 1d / numParticles))
    this.copy(particles = newParticles)
  }
}

object ParticleSeq {
  implicit val particleSetAsSparseFactor: SparseFactor[ParticleSeq,Double] = new SparseFactor[ParticleSeq,Double] {
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
      ParticleSeq(condVariables.toArray,condDomains.toArray,condParticles)
    }

    def defaultValue(f: ParticleSeq): Double = 0d

    def points(f: ParticleSeq): Map[WrappedArray[Int], Double] = f.particles.mapValues(_.sum)
  }

  def particleSeqToMap[T, A](particles: Seq[(Array[Int],Double)]): Map[WrappedArray[Int], Seq[Double]] = {
    particles.map(t => (wrapIntArray(t._1),t._2)).groupBy(_._1).map {
      case (k, vs) => k -> vs.map(_._2)
    }
  }

  def apply[A](factor: A, particles: Seq[(Array[Int],Double)])(implicit evF: Factor[A,Double]): ParticleSeq = {
    ParticleSeq(
      evF.variables(factor),
      evF.domains(factor),
      particleSeqToMap(particles)
    )
  }
}

