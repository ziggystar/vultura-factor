package vultura.util.graph

/**
 * A multi-way tree, also known as a rose tree. Also known as Cofree[Stream, A].
 */
sealed abstract class Tree[A] {

  import vultura.util.graph.Tree._

  /** The label at the root of this tree. */
  def rootLabel: A

  /** The child nodes of this tree. */
  def subForest: Stream[Tree[A]]

  /** A histomorphic transform. Each element in the resulting tree
    * is a function of the corresponding element in this tree
    * and the histomorphic transform of its children.
    **/
  def scanr[B](g: (A, Stream[Tree[B]]) => B): Tree[B] = {
    lazy val c = subForest.map(_.scanr(g))
    node(g(rootLabel, c), c)
  }

  /** A 2D String representation of this Tree, separated into lines. */
  def draw: Stream[String] = {
    def drawSubTrees(s: Stream[Tree[A]]): Stream[String] = s match {
      case Stream.Empty => Stream.Empty
      case Stream(t) => "|" #:: shift("`- ", " ", t.draw)
      case t #:: ts => "|" #:: shift("+- ", "| ", t.draw) append drawSubTrees(ts)
    }
    def shift(first: String, other: String, s: Stream[String]): Stream[String] =
      (first #:: Stream.continually(other)).zip(s).map {
        case (a, b) => a + b
      }

    rootLabel.toString #:: drawSubTrees(subForest)
  }

  def flatten: Stream[A] = rootLabel #:: subForest.flatMap(_.flatten)

  /** Binds the given function across all the subtrees of this tree. */
  def cobind[B](f: Tree[A] => B): Tree[B] = unfoldTree(this)(t => (f(t), () => t.subForest))

  def foldNode[Z](f: A => Stream[Tree[A]] => Z): Z =
    f(rootLabel)(subForest)

  def map[B](f: A => B): Tree[B] =
    node(f(rootLabel), subForest map (_ map f))

  def flatMap[B](f: A => Tree[B]): Tree[B] = {
    val r: Tree[B] = f(rootLabel)
    Tree.node(r.rootLabel, r.subForest #::: subForest.map(_.flatMap(f)))
  }

  def leafs: Stream[A] = this.scanr[Stream[A]]{
    case (a,Stream.Empty) => Stream(a)
    case (_,subTrees) => subTrees.flatMap(_.rootLabel)
  }.rootLabel

  def pushPathToLeafs(append: (A, A) => A): Tree[A] = this match {
    case Tree.Node(a,sf) => Tree.node(a,sf.map{st =>
      val Tree.Node(ax,sfx) = st
      Tree.node(append(ax,a), sfx).pushPathToLeafs(append)
    })
  }

  /** Propagate from root to leafs. */
  def scand[B,C](init: B)(f: (B,Tree[A]) => (C,Seq[B])): Tree[C] = {
    val (newVal, childPropagations) = f(init,this)
    node(newVal,this.subForest.zip(childPropagations).map{case (child,childProp) => child.scand(childProp)(f)})
  }

  /** Propagate form root to leafs. */
  def mapDown[B](init: B)(f: (B,A) => B): Tree[B] = {
    val mappedRootLabel = f(init, this.rootLabel)
    node(mappedRootLabel, this.subForest.map(_.mapDown(mappedRootLabel)(f)))
  }
}

object Tree {
  /** Construct a tree node with no children. */
  def apply[A](root: => A): Tree[A] = leaf(root)

  object Node {
    def unapply[A](t: Tree[A]): Option[(A, Stream[Tree[A]])] = Some((t.rootLabel, t.subForest))
  }

  def node[A](root: => A, forest: => Stream[Tree[A]]): Tree[A] = new Tree[A] {
    lazy val rootLabel = root
    lazy val subForest = forest

    override def toString = "<tree>"
  }

  /** Construct a tree node with no children. */
  def leaf[A](root: => A): Tree[A] = node(root, Stream.empty)

  def unfoldForest[A, B](s: Stream[A])(f: A => (B, () => Stream[A])): Stream[Tree[B]] =
    s.map(unfoldTree(_)(f))

  def unfoldTree[A, B](v: A)(f: A => (B, () => Stream[A])): Tree[B] =
    f(v) match {
      case (a, bs) => node(a, unfoldForest(bs.apply())(f))
    }
}
