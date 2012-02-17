package vultura.inference.sampling

import vultura.factors.SparseFactor
import collection.mutable.WrappedArray
import scalaz.Monoid

case class ParticleSet[T](variables: Array[Int],
                          domains: Array[Array[Int]],
                          particles: Map[WrappedArray[Int], T],
                          sumMonoid: Monoid[T])

object ParticleSet {
  implicit def particleSetAsSparseFactor[T]: SparseFactor[ParticleSet[T],T] = new SparseFactor[ParticleSet[T],T] {
    def variables(f: ParticleSet[T]): Array[Int] = f.variables
    def domains(f: ParticleSet[T]): Array[Array[Int]] = f.domains
    def condition(f: ParticleSet[T], variables: Array[Int], values: Array[Int]): ParticleSet[T] = {
      val (condVariables,condDomains) = (f.variables,f.domains).zipped.filterNot(t => variables.contains(t._1)).unzip
      //maps indices into f.variables to their demanded values
      val conditionMap: Array[(Int, Int)] = variables.map(f.variables.indexOf(_)).zip(values)
      val survivingIndices: Array[Int] = f.variables.zipWithIndex.filterNot(t => variables.contains(t._1)).map(_._2)
      def matchCondition(wa: WrappedArray[Int]): Boolean = conditionMap.forall{case (idx,v) => wa(idx) == v}
      val condParticles = f.particles.collect{
        case (wrappedArray,value) if(matchCondition(wrappedArray)) =>
          (wrapIntArray(survivingIndices.map(wrappedArray)),value)
      }
      ParticleSet(condVariables.toArray,condDomains.toArray,condParticles,f.sumMonoid)
    }

    def defaultValue(f: ParticleSet[T]): T = f.sumMonoid.zero

    def points(f: ParticleSet[T]): Map[WrappedArray[Int], T] = f.particles
  }
}

