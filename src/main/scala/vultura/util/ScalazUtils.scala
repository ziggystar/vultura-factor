package vultura.util

import scalaz.{Monoid, Tree}
import scalaz.Tree._

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
object ScalazUtils {
  /** Pimp scalaz.Tree. */
  implicit class RichTree[A](val tree: Tree[A]) extends AnyVal {

    def leafs: Stream[A] = tree.scanr[Stream[A]]{
      case (a,Stream.Empty) => Stream(a)
      case (_,subTrees) => subTrees.flatMap(_.rootLabel)
    }.rootLabel

    def pushPathToLeafs(ev: Monoid[A]): Tree[A] = tree match {
      case Tree.Node(a,sf) => Tree.node(a,sf.map{st =>
        val Tree.Node(ax,sfx) = st
        Tree.node(ev.append(ax,a), sfx).pushPathToLeafs(ev)
      })
    }

    /** Propagate from root to leafs. */
    def scand[B,C](init: B)(f: (B,Tree[A]) => (C,Seq[B])): Tree[C] = {
      val (newVal, childPropagations) = f(init,tree)
      node(newVal,tree.subForest.zip(childPropagations).map{case (child,childProp) => child.scand(childProp)(f)})
    }

    /** Propagate form root to leafs. */
    def mapDown[B](init: B)(f: (B,A) => B): Tree[B] = {
      val mappedRootLabel = f(init, tree.rootLabel)
      node(mappedRootLabel, tree.subForest.map(_.mapDown(mappedRootLabel)(f)))
    }
  }
}
