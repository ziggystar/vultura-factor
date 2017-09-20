package vultura.util

import scala.reflect.ClassTag

/**In contrast to CrossProductIndexer, a DomainCPI allows to specify the domains explicitly, which therefore are not
 * restricted to be integer ranges starting with zero.
 */
class DomainCPI[A: ClassTag](val domains: AA[A],val lsbf: Boolean = true) extends IndexedSeq[Array[A]]{
  val cpi = new CrossProductIndexer(domains.map(_.length),lsbf)
  def length: Int = cpi.length
  def apply(idx: Int): Array[A] = {
    val builder = new Array[A](domains.length)
    val plain = cpi(idx)
    var i = 0
    while(i < domains.length){
      builder(i) = domains(i)(plain(i))
      i += 1
    }
    builder
  }
  
  def index2Seq(idx: Int): Array[A] = apply(idx)

  def seq2Index(s: Array[A]): Int = {
    val indiced = new Array[Int](s.length)
    var i = 0
    while(i < indiced.length){
      indiced(i) = domains(i).indexOf(s(i))
      i += 1
    }
    cpi.seq2Index(indiced)
  }
}

case class IntDomainCPI(domains: AA[Int], lsbf: Boolean = true) extends IndexedSeq[Array[Int]]{
  private val cpi = new CrossProductIndexer(domains.map(_.length),lsbf)
  private val domainMap: Array[Option[Array[Int]]] = domains.map{ range =>
      //1 is good, 0 is bad
      val efficiency = range.length / range.max.toDouble
      if(efficiency < 0.2)
        None
      else
        Some((0 to range.max).map(range.indexOf(_)).toArray)
    }

  def length: Int = cpi.length
  def apply(idx: Int): Array[Int] = {
    val builder = new Array[Int](domains.length)
    val plain = cpi(idx)
    var i = 0
    while(i < domains.length){
      builder(i) = domains(i)(plain(i))
      i += 1
    }
    builder
  }

  /** @return domains(variable).indexOf(value). */
  def indexOfValue(variable: Int, value: Int) = domainMap(variable).map(_.apply(value))
    .getOrElse(domains(variable).indexOf(value))

  def seq2Index(s: Array[Int]): Int = {
    val indiced = new Array[Int](s.length)
    var i = 0
    while(i < indiced.length){
      indiced(i) = indexOfValue(i,s(i))
      i += 1
    }
    cpi.seq2Index(indiced)
  }

  def index2Seq(idx: Int): Array[Int] = apply(idx)
}