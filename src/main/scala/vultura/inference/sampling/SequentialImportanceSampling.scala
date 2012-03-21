package vultura.inference.sampling

import vultura.factors._
import util.Random
import collection.mutable.WrappedArray
import scalaz._
import Scalaz._
import vultura.util.{AbelianGroup, LogMeasure, Measure, RingWithZero}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 17.02.12
 */

object SequentialImportanceSampling {
  /** @return a set of samples and associated weights.    */
  def weightedImportanceSampling[A](factorSequence: Iterable[Seq[A]], numSamples: Int, random: Random, useLogDomain: Boolean = false)(implicit ev: Factor[A,Double]): ParticleSeq = {
    //draw initial samples
    implicit val ring: RingWithZero[Double] = if(useLogDomain) RingWithZero.logSumProd else RingWithZero.sumProduct
    implicit val sumMonoid = ring.addition
    implicit val measure = if(useLogDomain) LogMeasure else Measure.measureDouble

    val initialFactor: ProductFactor[A, Double] = ProductFactor(factorSequence.head, ring.multiplication)
    val initalParticleStream: Seq[(Array[Int], Double)] =
      initialFactor.partitionAndSample(random,measure,numSamples).get._2
        .map(a => a -> (1.toDouble / numSamples))

    val initialParticles = ParticleSeq(
      initialFactor,
      initalParticleStream,
      measure,
      sumMonoid
    )
    //weights are correct for initial samples
    val particles: ParticleSeq = factorSequence.tail.foldLeft(initialParticles){ case(oldParticles: ParticleSeq, newFactors: Seq[A]) =>
      //extend a particle x by drawing from newFactors | x exactly; weight remains unchanged,
      //multiply weight by partition function over new factors (probability of old sample given new distribution)
      val ParticleSeq(vars,doms,_) = oldParticles
      val newFactor = ProductFactor(newFactors, ring.multiplication)
      //draw `numSamples` new particles
      val unweightedParticles: Seq[(WrappedArray[Int], Double)] = oldParticles.generator(random).map{ oldAssignment =>
        val conditionedNewFactor: ProductFactor[A, Double] = newFactor.condition(vars, oldAssignment.toArray)
        conditionedNewFactor.partitionAndSample(random, measure, 1).map { case (conditionedPartition, sampleExtensions) =>
            assert(!conditionedPartition.isInfinite, "sampled invalid weight; factor values:\n%s".format(conditionedNewFactor.factors.map(evaluate(_,sampleExtensions.head)).mkString(", ")))
            (oldAssignment ++ sampleExtensions.head, conditionedPartition)
        }
      }.flatten.take(numSamples).toIndexedSeq

      println("%d distinct particles sampled" format unweightedParticles.map(_._1).distinct.size)

      new ParticleSeq(
        (vars ++ variables(newFactor)).distinct,
        (doms ++ domains(newFactor)).distinct,
        unweightedParticles,
        measure,
        sumMonoid
      )
    }

    particles
  }
}