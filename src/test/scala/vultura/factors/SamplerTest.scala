package vultura.factors

import org.specs2._
import specification.Fragments
import util.Random
import vultura.util.Measure

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 16.02.12
 */

class SamplerTest extends Specification {
  val factor1 = TableFactor.fromFunction(Seq(0, 1), Seq(Array(0, 1), Array(0, 1)), a => a.sum)

  def is: Fragments = {
    val random = new Random
    val numSamples = 1000
    implicit val measure = Measure.measureInt
    val samples = Seq.fill(numSamples)(sample(factor1, random))

    (samples.count(_.get.deep == Seq(0, 0)) === 0) and
      (samples.count(_.get.deep == Seq(0, 1)) / numSamples.toDouble must beCloseTo(0.25, 0.05)) and
      (samples.count(_.get.deep == Seq(1, 0)) / numSamples.toDouble must beCloseTo(0.25, 0.05)) and
      (samples.count(_.get.deep == Seq(1, 1)) / numSamples.toDouble must beCloseTo(0.5, 0.05))
  }
}
