package vultura.util.graph

/**
 * Created by thomas on 26.10.15.
 */
object BiSet{
  def unapplySeq[E](bs: BiSet[E]): Option[Seq[E]] = Some(Seq(bs.e1,bs.e2))
  def apply[E](e1: E, e2: E) = {
    require(e1 != e2)
    new BiSet(e1,e2)
  }
}

class BiSet[E] protected (val e1: E, val e2: E) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case BiSet(o1,o2) => (o1 == e1 && o2 == e2) || (o1 == e2 && o2 == e1)
    case _ => false
  }
  override def hashCode(): Int = Set(e1,e2).hashCode()
}