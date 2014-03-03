package vultura.util

import scalaz.{Monoid, Tree}

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
object ScalazUtils {
  implicit class RichTree[A](val t: Tree[A]) extends AnyVal {
    //move this somewhere else
    def leafs: Stream[A] = t.scanr[Stream[A]]{
      case (a,Stream.Empty) => Stream(a)
      case (_,subTrees) => subTrees.flatMap(_.rootLabel)
    }.rootLabel

    def pushPathToLeafs(ev: Monoid[A]): Tree[A] = t match {
      case Tree.Node(a,sf) => Tree.node(a,sf.map{st =>
        val Tree.Node(ax,sfx) = st
        Tree.node(ev.append(ax,a), sfx).pushPathToLeafs(ev)
      })
    }
  }
}
