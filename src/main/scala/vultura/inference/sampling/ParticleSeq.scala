package vultura.inference.sampling

import collection.mutable.WrappedArray
import scalaz.Monoid
import vultura.factors.{Factor, SparseFactor}

case class ParticleSeq[T](variables: Array[Int],
                          domains: Array[Array[Int]],
                          particles: Map[WrappedArray[Int], Seq[T]],
                          sumMonoid: Monoid[T]){
  assert(variables.distinct.size == variables.size, "double entries in `variables`")
}

object ParticleSeq {
  implicit def particleSetAsSparseFactor[T]: SparseFactor[ParticleSeq[T],T] = new SparseFactor[ParticleSeq[T],T] {
    def variables(f: ParticleSeq[T]): Array[Int] = f.variables
    def domains(f: ParticleSeq[T]): Array[Array[Int]] = f.domains
    def condition(f: ParticleSeq[T], variables: Array[Int], values: Array[Int]): ParticleSeq[T] = {
      val (condVariables,condDomains) = (f.variables,f.domains).zipped.filterNot(t => variables.contains(t._1)).unzip
      //maps indices into f.variables to their demanded values
      val conditionMap: Array[(Int, Int)] = variables.map(f.variables.indexOf(_)).zip(values)
      val survivingIndices: Array[Int] = f.variables.zipWithIndex.filterNot(t => variables.contains(t._1)).map(_._2)
      def matchCondition(wa: WrappedArray[Int]): Boolean = conditionMap.forall{case (idx,v) => wa(idx) == v}
      val condParticles = f.particles.collect{
        case (wrappedArray,value) if(matchCondition(wrappedArray)) =>
          (wrapIntArray(survivingIndices.map(wrappedArray)),value)
      }
      ParticleSeq(condVariables.toArray,condDomains.toArray,condParticles,f.sumMonoid)
    }

    def defaultValue(f: ParticleSeq[T]): T = f.sumMonoid.zero

    def points(f: ParticleSeq[T]): Map[WrappedArray[Int], T] = f.particles.mapValues(_.reduce(f.sumMonoid.append(_,_)))
  }

  def particleSeqToMap[T, A](particles: Seq[(Array[Int],T)]): Map[WrappedArray[Int], Seq[T]] = {
    particles.map(t => (wrapIntArray(t._1),t._2)).groupBy(_._1).map {
      case (k, vs) => k -> vs.map(_._2)
    }
  }

  def apply[A,T](factor: A, particles: Seq[(Array[Int],T)],sumMonoid: Monoid[T])(implicit evF: Factor[A,T]): ParticleSeq[T] = {
    ParticleSeq(
      evF.variables(factor),
      evF.domains(factor),
      particleSeqToMap(particles),
      sumMonoid
    )
  }
}

