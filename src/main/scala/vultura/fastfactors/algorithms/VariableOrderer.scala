package vultura.fastfactors.algorithms

import vultura.fastfactors.{Var, Problem}
import vultura.util.TreeWidth
import scala.util.Random

/** Computes a variable ordering for a given problem.
  * @author Thomas Geier <thomas.geier@uni-ulm.de>
  */
trait VariableOrderer extends (Problem => VariableOrder)

object VariableOrderer{
  def fromFunction(f: Problem => Seq[Int]) = new VariableOrderer {
    override def apply(v1: Problem): VariableOrder = VariableOrder(f(v1),v1)
  }
  def fromOrder(order: Seq[Int]) = new VariableOrderer {
    override def apply(v1: Problem): VariableOrder = VariableOrder(order, v1)
  }
}

case object MinDegreeOrderer extends VariableOrderer {
  override def apply(v1: Problem): VariableOrder =
    VariableOrder(TreeWidth.minDegreeOrdering(v1.factors.map(_.variables.toSet)).toIndexedSeq, v1)
}

case class RandomOrderer(seed: Long = 0) extends VariableOrderer{
  override def apply(v1: Problem): VariableOrder = VariableOrder(new Random(seed).shuffle(v1.variables.toSeq), v1)
}

case class VariableOrder(order: Seq[Var], problem: Problem)
