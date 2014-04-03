package vultura.fastfactors.algorithms

import vultura.fastfactors.{Var, Problem}
import vultura.util.TreeWidth

/** Computes a variable ordering for a given problem.
  * @author Thomas Geier <thomas.geier@uni-ulm.de>
  */
trait VariableOrderer extends (Problem => VariableOrder)

case object MinDegreeOrderer extends VariableOrderer {
  override def apply(v1: Problem): VariableOrder =
    VariableOrder(TreeWidth.minDegreeOrdering(v1.factors.map(_.variables.toSet)).toIndexedSeq, v1)
}

case class VariableOrder(order: Seq[Var], problem: Problem) extends Seq[Var]{
  override def apply(idx: Int): Var = order(idx)
  override def iterator: Iterator[Var] = order.iterator
  override def length: Int = order.length
}
