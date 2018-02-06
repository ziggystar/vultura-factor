package vultura.factor.generation

import org.specs2.mutable.Specification

class GeneratorTest extends Specification {
  "test multinomial" should {
    "1000 × 0.2/0.8 must have 200 times 0" in (Generator.multinomial(Array(1d,4)).replicate(1000).withSeed(0L).count(_ == 0) must beCloseTo(200,20))
    "1000 × 0.2/0.8 must have 800 times 1" in (Generator.multinomial(Array(1d,4)).replicate(1000).withSeed(0L).count(_ == 1) must beCloseTo(800,20))
  }

}
