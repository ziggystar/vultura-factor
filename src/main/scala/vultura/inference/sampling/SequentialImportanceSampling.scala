package vultura.inference.sampling

import vultura.factors._
import util.Random
import collection.mutable.WrappedArray
import scalaz.Monoid
import vultura.util.RingWithZero

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 17.02.12
 */

object SequentialImportanceSampling {
  /** @return a set of samples and associated weights.    */
  def weightedImportanceSampling[A](factorSequence: Iterable[Seq[A]], numSamples: Int, ring: RingWithZero[Double], random: Random)(implicit ev: Factor[A,Double]): ParticleSeq[Double] = {
    //draw initial samples
    val initialFactor: ProductFactor[A, Double] = ProductFactor(factorSequence.head, ring.multiplication)
    val initalParticleStream: Seq[(Array[Int], Double)] = IndexedSeq.fill(numSamples)(sample(initialFactor, random)).map(a => a -> (1.toDouble / numSamples))
    val initialParticles = ParticleSeq(
      initialFactor,
      initalParticleStream,
      implicitly[Monoid[Double]]
    )
    //weights are correct for initial samples
    val particles = factorSequence.tail.foldLeft(initialParticles){ case(remainingParticles,newFactors) =>
      //extend a particle x by drawing from newFactors | x exactly; weight remains unchanged,
      //multiply weight by partition function over new factors (probability of old sample given new distribution)
      val newFactor = ProductFactor(newFactors, ring.multiplication)
      val unweightedParticles: Seq[(WrappedArray[Int], Double)] = for(
        (oldAssignment,oldWeights) <- remainingParticles.particles.toSeq;
        oldWeight <- oldWeights
      ) yield {
        val conditionedNewFactor: ProductFactor[A, Double] = newFactor.condition(remainingParticles.variables,oldAssignment.toArray)
        val conditionedPartition = partition(conditionedNewFactor,ring.addition)
        val sampleExtension = sample(conditionedNewFactor,random)
        val newParticle = oldAssignment ++ sampleExtension
        (newParticle,oldWeight * conditionedPartition)
      }
      val normalizingConstant = unweightedParticles.map(_._2).sum
      //renormalized
      val normalizedParticles: Seq[(Array[Int], Double)] = unweightedParticles.map(t => (t._1.toArray,t._2/normalizingConstant))
      ParticleSeq(
        (remainingParticles.variables ++ variables(newFactor)).distinct,
        (remainingParticles.domains ++ domains(newFactor)).distinct,
        ParticleSeq.particleSeqToMap(normalizedParticles),
        remainingParticles.sumMonoid)
    }

    particles
  }
}