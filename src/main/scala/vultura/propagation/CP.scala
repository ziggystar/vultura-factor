package vultura.propagation

import vultura.util.SIIndex

import scala.collection.mutable
import scala.reflect.ClassTag

/** A calibration problem is a set of query nodes together with a set of rules.
  * Those nodes that are required for the computation, but are not provided by the rules are called *parameters*. */
case class CP[N <: Node : ClassTag, Impl](query: Iterable[N], rules: Seq[Rule[N,N,Impl]] = Seq()){
  lazy val nodes: Set[N] = Iterator
    .iterate(query.toSet)(ns => ns ++ ns.flatMap(n => depsOf(n).getOrElse(Seq())))
    .sliding(2).dropWhile(slide => slide(0).size != slide(1).size)
    .next().head
  /** @return Those nodes who are not covered by a rule. */
  lazy val parameters: Set[N] = nodes.filterNot(n => implementationOf(n).isDefined)
  /** @return The nodes that are needed for computing `n`. */
  def dependenciesOf(n: N): Option[IndexedSeq[N]] = compiled._2(compiled._1(n))
  /** @return The nodes that require `n` for their computation. */
  def descendantsOf(n: N): IndexedSeq[N] = compiled._3(compiled._1(n))
  def implementationOf(n: N): Option[Impl] = rules.find(_.isDefinedAt(n)).map(_.implementation(n))
  def addToQuery[N2 >: N <: Node : ClassTag](qs: Iterable[N2]): CP[N2,Impl] = widen[N2].copy(query = query ++ qs)
  def appendRule[N2 >: N <: Node : ClassTag, T <: N2: ClassTag,D <: N2](rule: Rule[T,D,Impl]): CP[N2,Impl] = widen[N2].appendRuleSameType(rule.widen[N2])
  private def appendRuleSameType(rule: Rule[N,N,Impl]) = this.copy(rules = rules :+ rule)
  def widen[N2 >: N <: Node : ClassTag] = CP[N2,Impl](query, rules.map(_.widen[N2]))

  /** Inefficient. */
  private def depsOf(n: N): Option[IndexedSeq[N]] = rules.find(_.isDefinedAt(n)).map(_.dependencies(n))

  lazy val compiled: (SIIndex[N],IndexedSeq[Option[IndexedSeq[N]]],IndexedSeq[IndexedSeq[N]]) = {
    val idx: SIIndex[N] = new SIIndex(nodes)
    val succsBuilders: mutable.IndexedSeq[List[N]] = mutable.IndexedSeq.fill[List[N]](idx.size)(Nil)
    val deps: IndexedSeq[Option[IndexedSeq[N]]] = idx.elements.zipWithIndex.map{ case (n,i) =>
      val ds: Option[IndexedSeq[N]] = depsOf(n)
      ds.foreach(_.foreach { d =>
        val di = idx(d)
        succsBuilders(di) = n :: succsBuilders(di)
      })
      ds
    }
    val succs = succsBuilders.map(_.toIndexedSeq)
    (idx,deps,succs)
  }
}
