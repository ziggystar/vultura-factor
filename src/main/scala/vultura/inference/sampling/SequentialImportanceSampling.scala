package vultura.inference.sampling

import vultura.factors._
import util.Random
import collection.mutable.WrappedArray
import collection.immutable.Map
import scalaz.Monoid
import collection.Seq
import vultura.util.{AbelianGroup, RingWithZero, Measure}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 17.02.12
 */

object SequentialImportanceSampling {
  /** @return a set of samples and associated weights. */
  def weightedImportanceSampling[A,R](factorSequence: Seq[Seq[A]], numSamples: Int, ring: RingWithZero[R], random: Random)(implicit ev: Factor[A,R],measure: Measure[R]): (ParticleSet[R],Double) = {
    //draw initial samples
    val initialFactor: ProductFactor[A, R] = ProductFactor(factorSequence.head, ring.multiplication)
    val initialParticles = ParticleSet(
      initialFactor.variables,
      initialFactor.domains,
      Seq.fill(numSamples)(sample(initialFactor, random)).map(a => wrapIntArray(a) -> (1.toDouble / numSamples)).toMap,
      implicitly[Monoid[Double]]
    )
    //weights are correct for initial samples

    factorSequence.tail.foldLeft(initialParticles){ case(particles,newFactors) =>
      //extend a particle x by drawing from newFactors | x exactly; weight remains unchanged,
      //obtaining the "unweighted" new particles
      val newFactor = ProductFactor(newFactors, ring.multiplication)
      val unweightedParticles: Map[WrappedArray[Int], Double] = particles.particles.map{ case (oldAssignment, oldWeight) =>
        val conditionedNewFactor = newFactor.condition(particles.variables,oldAssignment.toArray)
        val conditionedPartition = partition(conditionedNewFactor,ring.addition)
        val sampleExtension = sample(conditionedNewFactor,random)
        val newParticle = oldAssignment ++ sampleExtension
        (newParticle,oldWeight)
      }


    }
  }
}