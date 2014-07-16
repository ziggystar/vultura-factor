package vultura.factor

/**
 * Helpful stuff for creating primitive collections and FastFactor objects.
 */
object Utils {
  def AI(is: Int*): Array[Int] = is.toArray
  def DOMS(is: Int*) = is.toArray
  def VARS(is: Int*) = is.toArray
  def VALS(is: Double*) = is.toArray
  def SI(is: Int*): Seq[Int] = is.toSeq
  def AD(ds: Double*): Array[Double] = ds.toArray
  def AAI(iss: Array[Int]*): Array[Array[Int]] = iss.toArray
  def AAD(iss: Array[Double]*): Array[Array[Double]] = iss.toArray
  def IS[A](as: A*): IndexedSeq[A] = as.toIndexedSeq
  def FF(vs: Array[Int], values: Array[Double]) = Factor(vs,values)
}
