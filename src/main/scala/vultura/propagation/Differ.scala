package vultura.propagation

/**
 * Created by thomas on 30.01.15.
 */
trait Differ[-N <: Node, -R]{
  def diff(n: N, oldValue: R, newValue: R): Double
}

object MaxDiff extends Differ[NodeAD,Array[Double]]{
  override def diff(n: NodeAD, oldValue: Array[Double], newValue: Array[Double]): Double =
    oldValue.zip(newValue).map{case (x,y) => math.abs(x - y)}.max
}
