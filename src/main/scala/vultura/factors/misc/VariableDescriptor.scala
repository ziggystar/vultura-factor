package vultura.factors.misc

import scalaz.Monoid

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 28.02.12
 */

trait VariableDescriptor[A] {
  def description: Map[Int,A]
}

object VariableDescriptor{
  implicit def vdAsMonoid[A]: Monoid[VariableDescriptor[A]] = new Monoid[VariableDescriptor[A]]{
    def append(s1: VariableDescriptor[A], s2: => VariableDescriptor[A]): VariableDescriptor[A] = new VariableDescriptor[A] {
      def description: Map[Int, A] = s1.description ++ s2.description
    }
    val zero: VariableDescriptor[A] = new VariableDescriptor[A] {
      def description: Map[Int, A] = Map.empty
    }
  }
}
