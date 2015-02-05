package vultura.propagation

/**
 * Created by thomas on 30.01.15.
 */
trait Differ[-N <: Node]{
  def diff(n: N, oldValue: N#TImpl, newValue: N#TImpl): Double
}

object MaxDiff extends Differ[NodeAD]{
  override def diff(n: NodeAD, oldValue: Array[Double], newValue: Array[Double]): Double = {
    var i = 0
    var max = Double.MinValue
    while(i < oldValue.length){
      val d = math.abs(oldValue(i) - newValue(i))
      if(d > max)
        max = d
      i += 1
    }
    max
  }
}
