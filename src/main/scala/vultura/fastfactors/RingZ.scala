package vultura.fastfactors

/**
 * Type class describing ring properties of a type.
 */
trait RingZ[@specialized(Double) T]{
  def zero: T
  def one: T
  def sum(s1: T, s2: T): T
  def prod(f1: T, f2: T): T
  def sumA(ss: Array[T]): T = ss.foldLeft(zero)(sum)
  def prodA(fs: Array[T]): T = fs.foldLeft(one)(prod)
}

object NormalD extends RingZ[Double]{
  final val zero: Double = 0d
  final val one: Double = 1d
  def sum(s1: Double, s2: Double): Double = s1 + s2
  def prod(f1: Double, f2: Double): Double = f1 * f2
  override def sumA(ss: Array[Double]): Double = {
    var i = 0
    var result = zero
    while(i < ss.size){
      result += ss(i)
      i += 1
    }
    result
  }
  override def prodA(fs: Array[Double]): Double = {
    var i = 0
    var result = one
    while(i < fs.size){
      result *= fs(i)
      i += 1
    }
    result
  }
}

object LogD extends RingZ[Double] {
  final val zero: Double = Double.NegativeInfinity
  final val one: Double = 0d

  def sum(s1: Double, s2: Double): Double =
    if(Double.NegativeInfinity == s1)
      s2
    else if(s2 == Double.NegativeInfinity)
      s1
    else if(math.abs(s1 - s2) > math.abs(s2-s1))
      s1 + math.log1p(math.exp(s2 - s1))
    else
      s2 + math.log1p(math.exp(s1 - s2))

  def prod(f1: Double, f2: Double): Double = f1 + f2

  override def prodA(fs: Array[Double]): Double = {
    var i = 0
    var result = one
    while(i < fs.size){
      result += fs(i)
      i += 1
    }
    result
  }
}
