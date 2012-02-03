package vultura.util

class DomainCPI[@specialized A: ClassManifest](_domains: AA[A]) {
  val cpi = new CrossProductIndexer(_domains.map(_.size))

  def size = cpi.size

  def iterator: Iterator[Array[A]] = cpi.iterator.map(ints => ints.zip(_domains).map(t => t._2(t._1)))
}
