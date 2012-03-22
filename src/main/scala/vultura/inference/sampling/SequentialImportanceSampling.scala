package vultura.inference.sampling

import vultura.factors._
import util.Random
import collection.mutable.WrappedArray
import vultura.util.{LogMeasure, Measure, RingWithZero}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 17.02.12
 */

object SequentialImportanceSampling {
  /** @return a set of samples and associated weights.    */
  def weightedImportanceSampling[A](factorSequence: Iterable[Seq[A]],
                                    numSamples: Int,
                                    random: Random,
                                    useLogDomain: Boolean = false,
                                    usePreweighing: Boolean = true)(implicit ev: Factor[A,Double]): ParticleSeq = {
    //draw initial samples
    implicit val ring: RingWithZero[Double] = if(useLogDomain) RingWithZero.logSumProd else RingWithZero.sumProduct
    implicit val measure = if(useLogDomain) LogMeasure else Measure.measureDouble

    val initialFactor: ProductFactor[A, Double] = ProductFactor(factorSequence.head, ring.multiplication)
    val initalParticleStream: Seq[(Array[Int], Double)] =
      initialFactor.partitionAndSample(random,measure,numSamples).get._2
        .map(a => a -> (1.toDouble / numSamples))

    val initialParticles = ParticleSeq(
      initialFactor,
      initalParticleStream,
      measure
    )
    var slice = 1
    //weights are correct for initial samples
    val particles: ParticleSeq = factorSequence.tail.foldLeft(initialParticles){ case(oldParticles: ParticleSeq, newFactors: Seq[A]) =>
      println("Sampling slice %d".format(slice))
      slice += 1
      //extend a particle x by drawing from newFactors | x exactly; weight remains unchanged,
      //multiply weight by partition function over new factors (probability of old sample given new distribution)
      val ParticleSeq(vars,doms,_) = oldParticles
      val newFactor = ProductFactor(newFactors, ring.multiplication)

      //reweigh the particles using the partition function of the extension
      lazy val reweighedParticles: ParticleSeq = oldParticles.multiplyWeight(
       oldParticle => newFactor.condition(vars, oldParticle.toArray).partitionAndSample(random,measure,0).map(_._1).getOrElse(measure.sum.zero),
        ring.multiplication).normalized

      println("%d of %d particles have a weight larger than %f".format(
        reweighedParticles.particles.count(p => measure.weight(p._2) > (1.toDouble/numSamples)),
        reweighedParticles.particles.size,
        1.toDouble/numSamples))

      def reweighedExtendedParticles = reweighedParticles.generate(random,numSamples).get.par.map{ oldAssignment =>
        val conditionedNewFactor: ProductFactor[A, Double] = newFactor.condition(vars, oldAssignment.toArray)
        val sampleExtension = conditionedNewFactor.partitionAndSample(random, measure, 1).get._2.head //tihs must work, since partition can't be one
        (oldAssignment ++ sampleExtension, 1d)
      }.seq

      //draw `numSamples` new particles
      def newParticles: Seq[(WrappedArray[Int], Double)] = Iterator.continually(oldParticles.generate(random,1).get.head).map{ oldAssignment =>
        val conditionedNewFactor: ProductFactor[A, Double] = newFactor.condition(vars, oldAssignment.toArray)
        conditionedNewFactor.partitionAndSample(random, measure, 1).map { case (conditionedPartition, sampleExtensions) =>
            assert(!conditionedPartition.isInfinite, "sampled invalid weight; factor values:\n%s".format(conditionedNewFactor.factors.map(evaluate(_,sampleExtensions.head)).mkString(", ")))
            (oldAssignment ++ sampleExtensions.head, conditionedPartition)
        }
      }.flatten.take(numSamples).toIndexedSeq

      val finalParticles = if(usePreweighing) reweighedExtendedParticles else newParticles
      println("%d distinct particles sampled" format finalParticles.map(_._1).distinct.size)

      new ParticleSeq(
        (vars ++ variables(newFactor)).distinct,
        (doms ++ domains(newFactor)).distinct,
        finalParticles,
        measure
      )
    }

    particles
  }
}