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

/** This ring only accepts the array invocations with a single element and returns this singe element.
  * It can be used where the true ring is not known.
  */
object SafeD extends RingZ[Double]{
  def zero: Double = sys.error("Safe ring has no zero")
  def one: Double = sys.error("Safe ring has no one")
  def sum(s1: Double, s2: Double): Double = sys.error("safe ring has no binary ops")
  def prod(f1: Double, f2: Double): Double = sys.error("safe ring has no binary ops")
  override def sumA(ss: Array[Double]): Double =
    if(ss.size == 1) ss(0) else sys.error("safe ring accepts only unit arrays")
  override def prodA(fs: Array[Double]): Double =
    if(fs.size == 1) fs(0) else sys.error("safe ring accepts only unit arrays")
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

  @inline
  def sum(s1: Double, s2: Double): Double =
    if(s1.isNegInfinity)
      s2
    else if(s2.isNegInfinity)
      s1
    else if(s1 > s2)
      s1 + math.log1p(math.exp(s2 - s1))
    else
      s2 + math.log1p(math.exp(s1 - s2))

  def prod(f1: Double, f2: Double): Double = f1 + f2

  override def sumA(ss: Array[Double]): Double = {
    if(ss.length == 2)
      return this.sum(ss(0), ss(1))

    var max = ss(0)
    var maxi = 0
    var i = 0

    while(i < ss.size){
      if(ss(i) > max){
        max = ss(i)
        maxi = i
      }
      i += 1
    }

    //we cannot substract -Inf from -Inf below
    if(max.isNegInfinity)
      return Double.NegativeInfinity

    i = 0
    var sum = 0d
    val thresh = Double.NegativeInfinity
    while(i < ss.size){
      if(i != maxi || ss(i) < thresh){
        sum += math.exp(ss(i) - max)
      }
      i += 1
    }
    max + (if(sum == 0d) 0 else math.log1p(sum))
  }

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
