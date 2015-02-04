package vultura.propagation

/**
 * Created by thomas on 30.01.15.
 */
trait Differ[-N <: Node]{
  def diff(n: N, oldValue: N#TImpl, newValue: N#TImpl): Double
}

object MaxDiff extends Differ[NodeAD]{
  override def diff(n: NodeAD, oldValue: Array[Double], newValue: Array[Double]): Double =
    oldValue.zip(newValue).map{case (x,y) => math.abs(x - y)}.max
}
