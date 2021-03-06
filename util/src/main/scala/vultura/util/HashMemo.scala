package vultura.util

/**
 * Mixin for memoizing hash values of case classes (more generally products).
 * @see scala.Product
 */
trait HashMemo {self: Product =>
  override final val hashCode: Int = self.productIterator.toSeq.hashCode()
}
