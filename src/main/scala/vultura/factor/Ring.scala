package vultura.factor

import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec

/**
 * Type class describing ring properties of a type.
 */
trait Ring[@specialized(Double) T]{
  def zero: T
  def one: T
  def productInverse(x: T): T
  def sum(s1: T, s2: T): T
  def prod(f1: T, f2: T): T
  def sumA(ss: Array[T]): T = ss.foldLeft(zero)(sum)
  def prodA(fs: Array[T]): T = fs.foldLeft(one)(prod)
  //for the the following methods it's not so clear how to generalize to rings
  //in particular, the result type is currently fixed to be Double in normal representation.
  def normalize(a: Array[T]): Array[T] = sys.error("not supported")
  def normalizeInplace(a: Array[T]): Unit = sys.error("not supported")
  /** @return In normal representation (not log). */
  def maxNorm(a: Array[T], b: Array[T]): Double = sys.error("not supported")
  /** @return In normal representation (not log). */
  def entropy(a: Array[T]): Double = sys.error("not supported")
  /** @return In normal representation (not log). */
  def expectation(p: Array[T], f: Array[T]): Double = sys.error("not supported")
  /** @return the expectation of the second array given the measure of the first, as both arrays are in the given encoding. */
  def logExpectation(p: Array[T], f: Array[T]): Double = sys.error("not supported")
  def decode(p: Array[T]): Array[Double] = sys.error("not supported")
  def encode(p: Array[Double]): Array[T] = sys.error("not supported")
  def log(x: T): Double = sys.error("not supported")
  def pow(x: T, e: Double): T = sys.error("not supported")
  def div(n: T, d: T): T = sys.error("not supported")
}

object Ring{
  /** @return product of the arguments. If the first one is zero, zero is returned (wins against infinity). */
  @inline
  final def safeProd(x1: Double, x2: Double): Double = if(x1 == 0) 0 else x1 * x2
}

/** This ring only accepts the array invocations with a single element and returns this singe element.
  * It can be used where the true ring is not known.
  */
object SafeD extends Ring[Double]{
  def productInverse(x: Double): Double = sys.error("Safe ring has no product inverse")
  def zero: Double = sys.error("Safe ring has no zero")
  def one: Double = sys.error("Safe ring has no one")
  def sum(s1: Double, s2: Double): Double = sys.error("safe ring has no binary ops")
  def prod(f1: Double, f2: Double): Double = sys.error("safe ring has no binary ops")
  override def sumA(ss: Array[Double]): Double =
    if(ss.length == 1) ss(0) else sys.error("safe ring accepts only unit arrays")
  override def prodA(fs: Array[Double]): Double =
    if(fs.length == 1) fs(0) else sys.error("safe ring accepts only unit arrays")

  override def toString: String = "no-op ring"
}

object NormalD extends Ring[Double]{
  val zero: Double = 0d
  val one: Double = 1d
  def productInverse(x: Double): Double = 1/x
  def sum(s1: Double, s2: Double): Double = s1 + s2
  def prod(f1: Double, f2: Double): Double = f1 * f2
  override def sumA(ss: Array[Double]): Double = {
    var i = 0
    var result = zero
    while(i < ss.length){
      result += ss(i)
      i += 1
    }
    result
  }
  override def prodA(fs: Array[Double]): Double = {
    var i = 0
    var result = one
    while(i < fs.length){
      result *= fs(i)
      i += 1
    }
    result
  }

  override def maxNorm(a: Array[Double], b: Array[Double]): Double = a.zip(b).map(t => math.abs(t._1 - t._2)).max

  override def normalize(a: Array[Double]): Array[Double] = {
    val sum = sumA(a)
    if(sum == zero) a.clone()
    else a.map(_ / sum)
  }

  override def decode(p: Array[Double]): Array[Double] = p

  override def encode(p: Array[Double]): Array[Double] = p

  override def normalizeInplace(a: Array[Double]) {
    val z = sumA(a)
    if(z == zero)
      return
    var i = 0
    while(i < a.length){
      a(i) = a(i) / z
      i += 1
    }
  }

  /** @return In normal representation (not log). */
  override def entropy(a: Array[Double]): Double = {
    var i = 0
    var e = 0d
    while(i < a.length){
      if(a(i) != 0)
        e -= Ring.safeProd(a(i), math.log(a(i)))
      i += 1
    }
    e
  }

  /** @return In normal representation (not log). */
  @tailrec
  final def expectationR(p: Array[Double], f: Array[Double], accZ: Double = 0d, i: Int = 0, acc: Double = 0): Double =
    if(i < p.length) expectationR(p,f,accZ + p(i),i + 1, acc + Ring.safeProd(p(i), f(i))) else
      if(accZ == 0) 0d else acc/accZ

  /** @return In normal representation (not log). */
  override def expectation(p: Array[Double], f: Array[Double]): Double = expectationR(p,f)

  /** @return the expectation of the second array given the measure of the first, as both arrays are in the given encoding. */
  override def logExpectation(p: Array[Double], f: Array[Double]): Double = expectation(p, f.map(math.log))

  override def log(x: Double): Double = math.log(x)

  override def pow(x: Double, e: Double): Double = math.pow(x,e)

  override def div(n: Double, d: Double): Double = n / d

  override def toString: String = "normal domain"
}

object LogD extends Ring[Double] with LazyLogging {
  final val zero: Double = Double.NegativeInfinity
  final val one: Double = 0d

  override def productInverse(x: Double): Double = -x

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
    if(ss.length == 0)
      return zero
    if(ss.length == 1)
      return ss(0)
    else if(ss.length == 2)
      return this.sum(ss(0), ss(1))

    var max = ss(0)
    var maxi = 0
    var i = 0

    while(i < ss.length){
      if(ss(i) > max){
        max = ss(i)
        maxi = i
      }
      i += 1
    }

    //we cannot subtract -Inf from -Inf below
    if(max.isNegInfinity)
      return Double.NegativeInfinity

    i = 0
    var sum = 0d
    val thresh = Double.NegativeInfinity
    while(i < ss.length){
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
    while(i < fs.length){
      result += fs(i)
      i += 1
    }
    result
  }

  override def normalize(a: Array[Double]): Array[Double] = {
    val z = sumA(a)
    if(z == zero) a.clone()
    else a.map(_ - z)
  }


  override def normalizeInplace(a: Array[Double]) {
    val z = sumA(a)
    if(z == zero)
      logger.error("normalizing null quantity", a)
    var i = 0
    while(i < a.length){
      a(i) -= z
      i += 1
    }
  }

  /** @return In normal representation (not log). */
  override def entropy(a: Array[Double]): Double = {
    val normalized = normalize(a)
    normalized.foldLeft(0d){case (h, lnp) => h - Ring.safeProd(math.exp(lnp), lnp)}
  }

  /** @return In normal representation (not log). */
  override def expectation(p: Array[Double], f: Array[Double]): Double = {
    val normalized = normalize(p)
    normalized.zip(f).foldLeft(0d){case (e,(lnp,fv)) => e + Ring.safeProd(math.exp(lnp), fv)}
  }

  /** @return the expectation of the second array given the measure of the first, as both arrays are in the given encoding. */
  override def logExpectation(p: Array[Double], f: Array[Double]): Double = expectation(p, f)

  override def decode(p: Array[Double]): Array[Double] = p.map(math.exp)
  override def encode(p: Array[Double]): Array[Double] = p.map(math.log)
  override def log(x: Double): Double = x
  override def pow(x: Double, e: Double): Double = x * e
  override def div(n: Double, d: Double): Double = n - d
  override def toString: String = "log domain"
}
