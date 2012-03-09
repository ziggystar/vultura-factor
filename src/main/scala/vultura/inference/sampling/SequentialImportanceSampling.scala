package vultura.inference.sampling

import vultura.factors._
import util.Random
import vultura.util.RingWithZero

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 17.02.12
 */

object SequentialImportanceSampling {
  /** @return a set of samples and associated weights.    */
  def weightedImportanceSampling[A](factorSequence: Iterable[Seq[A]], numSamples: Int, random: Random)(implicit ev: Factor[A,Double]): ParticleSeq = {
    //draw initial samples
    val initialFactor: ProductFactor[A, Double] = ProductFactor(factorSequence.head, RingWithZero.sumProduct[Double].multiplication)
    val initalParticleStream: Seq[(Array[Int], Double)] = IndexedSeq.fill(numSamples)(initialFactor.partitionAndSample(random,RingWithZero.sumProduct[Double].addition)._2).map(a => a -> (1.toDouble / numSamples))
    val initialParticles = ParticleSeq(
      initialFactor,
      initalParticleStream
    )
    //weights are correct for initial samples
    val particles = factorSequence.tail.foldLeft(initialParticles){ case(remainingParticles,newFactors) =>
      //extend a particle x by drawing from newFactors | x exactly; weight remains unchanged,
      //multiply weight by partition function over new factors (probability of old sample given new distribution)
      val ParticleSeq(vars,doms,_) = remainingParticles
      val resampledParticles = remainingParticles.resample(numSamples,random)
      val newFactor = ProductFactor(newFactors, RingWithZero.sumProduct[Double].multiplication)
      val unweightedParticles = for(
        (oldAssignment,oldWeights) <- resampledParticles.particles.toSeq;
        oldWeight <- oldWeights
      ) yield {
        val conditionedNewFactor: ProductFactor[A, Double] = newFactor.condition(vars,oldAssignment.toArray)
        val (conditionedPartition, sampleExtension) = conditionedNewFactor.partitionAndSample(random,RingWithZero.sumProduct[Double].addition)
        //this is the old/brute force code
//        val conditionedPartition = partition(conditionedNewFactor, RingWithZero.sumProduct[Double].addition)
//        val sampleExtension = sample(conditionedNewFactor,random)
        val newParticle = oldAssignment ++ sampleExtension
        (newParticle,oldWeight * conditionedPartition)
      }
      val normalizingConstant = unweightedParticles.map(_._2).sum
      //renormalized
      val normalizedParticles = unweightedParticles.map(t => (t._1.toArray,t._2/normalizingConstant))
      ParticleSeq(
        (vars ++ variables(newFactor)).distinct,
        (doms ++ domains(newFactor)).distinct,
        ParticleSeq.particleSeqToMap(normalizedParticles))
    }

    particles
  }
}