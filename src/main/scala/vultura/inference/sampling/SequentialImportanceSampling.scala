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
    implicit val logMeasure = if(useLogDomain) LogMeasure else Measure.measureDouble

    val initialFactor: ProductFactor[A, Double] = ProductFactor(factorSequence.head, ring.multiplication)
    val initalParticleStream: Seq[(Array[Int], Double)] =
      Iterator.continually(initialFactor.partitionAndSample(random,ring.addition)._2)
        .flatten
        .map(a => a -> (1.toDouble / numSamples))
        .take(numSamples)
        .toIndexedSeq

    val initialParticles = ParticleSeq(
      initialFactor,
      initalParticleStream,
      logMeasure,
      sumMonoid
    )
    //weights are correct for initial samples
    val particles: ParticleSeq = factorSequence.tail.foldLeft(initialParticles){ case(oldParticles,newFactors) =>
      //extend a particle x by drawing from newFactors | x exactly; weight remains unchanged,
      //multiply weight by partition function over new factors (probability of old sample given new distribution)
      val ParticleSeq(vars,doms,_) = oldParticles
      val newFactor = ProductFactor(newFactors, ring.multiplication)
      //draw `numSamples` new particles
      val unweightedParticles: Seq[(WrappedArray[Int], Double)] = oldParticles.generator(random).map{ oldAssignment =>
        val conditionedNewFactor: ProductFactor[A, Double] = newFactor.condition(vars, oldAssignment.toArray)
        val (conditionedPartition, sampleExtension) = conditionedNewFactor.partitionAndSample(random, ring.addition)
//        print(sampleExtension.map(ext => (oldAssignment  ++ ext).mkString(",") + " p= " + conditionedPartition).getOrElse("no sample found 2"))
        sampleExtension.map {
          sE =>
            assert(!conditionedPartition.isInfinite, "sampled invalid weight; factor values:\n%s".format(conditionedNewFactor.factors.map(evaluate(_,sE)).mkString(", ")))
//            println(".")
            val newParticle = oldAssignment ++ sE
            (newParticle, conditionedPartition)
        }
      }.flatten.take(numSamples).toIndexedSeq

      println(":step\n")
      //println("%d distinct particles sampled" format unweightedParticles.map(_._1).distinct.size)

      new ParticleSeq(
        (vars ++ variables(newFactor)).distinct,
        (doms ++ domains(newFactor)).distinct,
        unweightedParticles,
        logMeasure,
        sumMonoid
      )
    }

    particles
  }
}