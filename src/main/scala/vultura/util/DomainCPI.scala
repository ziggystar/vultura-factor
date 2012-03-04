package vultura.util


class DomainCPI[A: ClassManifest](val domains: AA[A],val lsbf: Boolean = true) extends IndexedSeq[Array[A]]{
  val cpi = new CrossProductIndexer(domains.map(_.size),lsbf)
  def length: Int = cpi.length
  def apply(idx: Int): Array[A] = {
    val builder = new Array[A](domains.size)
    val plain = cpi(idx)
    var i = 0
    while(i < domains.size){
      builder(i) = domains(i)(plain(i))
      i += 1
    }
    builder
  }
}
