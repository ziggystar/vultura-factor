package vultura.util


class DomainCPI[A: ClassManifest](val domains: AA[A],val lsbf: Boolean = true) extends IndexedSeq[Array[A]]{
  val cpi = new CrossProductIndexer(domains.map(_.size),lsbf)
  def length: Int = cpi.length
  def apply(idx: Int): Array[A] = cpi(idx).zip(domains).map(t => t._2(t._1))
}
