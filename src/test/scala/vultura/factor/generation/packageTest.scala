package vultura.factor.generation


import org.specs2.mutable.Specification
import vultura.factor.LabeledProblem

import scala.util.Random

class packageTest extends Specification {

  "adding magnetic field to 2x2 grid problem must yield 4 singleton factors" >> {
      pottsGrid(Seq(2 -> true, 2 -> true), 2, Generator.only(0)).flatMap(withMagneticField(_, Generator.only(0))).generate(new Random(0)).problem.factors.count(_.variables.size == 1) === 4
  }

  "testing new DSL" >> {
    def p: LabeledProblem[IndexedSeq[Int]] = problemGenerator(Generator.only(graph.lattice(2 -> true, 2 -> true)), FixedDomainsSize(2), IIDValuedParam(Generator.uniform(0, 1))).generate(new Random(0))
    p must throwA[Throwable].not
  }
}
