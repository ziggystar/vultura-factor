package vultura.util


class DomainCPI[@specialized(Byte,Short,Int,Long,Double) A: ClassManifest](val domains: AA[A]) extends IndexedSeq[Array[A]]{
  val cpi = new CrossProductIndexer(domains.map(_.size))
  def length: Int = cpi.length
  def apply(idx: Int): Array[A] = cpi(idx).zip(domains).map(t => t._2(t._1))
}
