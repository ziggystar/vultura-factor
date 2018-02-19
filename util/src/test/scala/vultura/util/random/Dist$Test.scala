package vultura.util.random

import java.util.{Random => JRandom}

import org.specs2.mutable.Specification

import scala.util.Random

class Dist$Test extends Specification {

  "implicit conversion from Scala random must work" >> {
    Dist.bernoulli(0.5)(new Random(0)) must throwA[Exception].not
  }

  "implicit conversion from Java random must work" >> {
    Dist.bernoulli(0.5)(new JRandom(0)) must throwA[Exception].not
  }

  "multinomial test" >> {
    Dist.multinomial(0d -> 1, 0d -> 2, 1d -> 3).repeat(1000).map(_.sum)(new Random(0)) must beCloseTo(500d,10)
  }
}
