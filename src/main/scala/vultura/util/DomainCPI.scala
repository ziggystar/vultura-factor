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
  
  def index2Seq(idx: Int): Array[A] = apply(idx)

  def seq2Index(s: Array[A]): Int = {
    val indiced = new Array[Int](s.size)
    var i = 0
    while(i < indiced.size){
      indiced(i) = domains(i).indexOf(s(i))
      i += 1
    }
    cpi.seq2Index(indiced)
  }
}
