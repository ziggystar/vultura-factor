package vultura.inference.sampling

import vultura.factors._
import util.Random
import collection.mutable.WrappedArray
import scalaz._
import Scalaz._
import vultura.util.{Measure, RingWithZero}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 17.02.12
 */

object SequentialImportanceSampling {
  /** @return a set of samples and associated weights.    */
  def weightedImportanceSampling[A](factorSequence: Iterable[Seq[A]], numSamples: Int, random: Random)(implicit ev: Factor[A,Double]): ParticleSeq = {
    //draw initial samples
    implicit val ring = RingWithZero.logSumProd
    implicit val sumMonoid = ring.addition
    implicit val logMeasure = new Measure[Double]{def weight(a: Double*): Double = a.map(math.exp).sum}

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
    val particles: ParticleSeq = factorSequence.tail.foldLeft(initialParticles){ case(remainingParticles,newFactors) =>
      //extend a particle x by drawing from newFactors | x exactly; weight remains unchanged,
      //multiply weight by partition function over new factors (probability of old sample given new distribution)
      val ParticleSeq(vars,doms,_) = remainingParticles
      val newFactor = ProductFactor(newFactors, ring.multiplication)
      //draw `numSamples` new particles
      val unweightedParticles: Seq[(WrappedArray[Int], Double)] = remainingParticles.generator(random).map{ oldAssignment =>
        val conditionedNewFactor: ProductFactor[A, Double] = newFactor.condition(vars, oldAssignment.toArray)
        val (conditionedPartition, sampleExtension) = conditionedNewFactor.partitionAndSample(random, ring.addition)
        if(!sampleExtension.isDefined)
          println("no sample found 2")
        sampleExtension.map {
          sE =>
            assert(!conditionedPartition.isInfinite, "sampled invalid weight; factor values:\n%s".format(conditionedNewFactor.factors.map(evaluate(_,sE)).mkString(", ")))
            val newParticle = oldAssignment ++ sE
            (newParticle, conditionedPartition)
        }
      }.flatten.take(numSamples).toSeq

      println("%d distinct particles sampled" format unweightedParticles.map(_._1).distinct.size)

      val normalizingConstant = unweightedParticles.map(_._2).sum
      //renormalized
      val normalizedParticles = unweightedParticles.map(t => (t._1.toArray,t._2/normalizingConstant))
      new ParticleSeq(
        (vars ++ variables(newFactor)).distinct,
        (doms ++ domains(newFactor)).distinct,
        normalizedParticles map ((wrapIntArray _) <-: _),
        logMeasure,
        sumMonoid
      )
    }

    particles
  }
}