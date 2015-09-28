package vultura.factor.generation

import org.specs2.Specification

import scala.util.Random

/**
 * Created by thomas on 28.09.15.
 */
class packageTest extends Specification {

  def is = s2"""adding magnetic field to 2x2 grid problem must yield 4 singleton factors
                ${pottsGrid(Seq(2 -> true, 2 -> true), 2, Constant(0)).flatMap(withMagneticField(_,Constant(0))).generate(new Random(0)).problem.factors.count(_.variables.size == 1) === 4}
  """
}
