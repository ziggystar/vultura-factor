package vultura.fastfactors.algorithms.conditioned

import vultura.util._
import scalaz._

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
sealed trait LScheme {
  def usedVariables: Set[Int]

  /** used decisions from partial assignment to strip off contradictory assignments.
    * - `!strip(pa).usedVariables.contains(pa.values)` holds. */
  def condition(pa: Map[Int,Int]): LScheme

  /** Adds other to every leaf. */
  def multiply(other: LScheme): LScheme = this match {
    case DCon(Stream.Empty) => other
    case DCon(sf)           => DCon(sf.map(_.multiply(other)))
    case d@DDis(_,subs)     => d.copy(assignments = subs.map{case (k,v) => k -> v.multiply(other)})
  }

  /** Gets rid of the DCon nodes. */
  def linearize: Tree[Option[(Int,Int)]] = this match {
    case DCon(Stream.Empty) => Tree.node(None,Stream())
    case DCon(component #:: rest) => DCon(rest).multiply(component).linearize
    case DDis(v,as) => Tree.node(None,as.toStream.map{case (value,subScheme) =>
      val linSubTree = subScheme.linearize
      Tree.node(Some(v -> value),linSubTree.subForest)
    })
  }

  def partialAssignments: Stream[Map[Int, Int]] = {
    import scalaz.Scalaz._
    import vultura.util.ScalazUtils._
    linearize.map(_.fold(Map[Int,Int]())(Map(_))).pushPathToLeafs(implicitly[Monoid[Map[Int,Int]]]).leafs
  }
}

object LScheme{
  def empty: LScheme = DCon()
  /** The scheme that just splits the given variable. */
  def split(v: Int, domains: Array[Int]): LScheme = DDis(v,(0 until domains(v)).map(_ -> empty)(collection.breakOut))
}

/** Corresponds to decomposable conjunction for d-dnnf. */
case class DCon(subForest: Stream[LScheme]) extends LScheme {
  require((subForest.map(_.usedVariables).toSet: Set[Set[Int]]).isPairwiseDisjoint)

  override def usedVariables: Set[Int] = subForest.flatMap(_.usedVariables)(collection.breakOut)

  /** used decisions from partial assignment to strip off contradictory assignments.
    * - `!strip(pa).usedVariables.contains(pa.values)` holds. */
  override def condition(pa: Map[Int, Int]): LScheme = DCon(subForest.map(_.condition(pa)))
}
object DCon{
  def apply(lss: LScheme*): DCon = DCon(lss.toStream)
}

/** Corresponds do deterministic disjunction. */
case class DDis(variable: Int, assignments: Map[Int,LScheme]) extends LScheme {
  require(assignments.values.forall(!_.usedVariables.contains(variable)), "trying to build double assignment")

  override def usedVariables: Set[Int] =
    (assignments.values.flatMap(_.usedVariables)(collection.breakOut): Set[Int]) + variable

  /** used decisions from partial assignment to strip off contradictory assignments.
    * - `!strip(pa).usedVariables.contains(pa.values)` holds. */
  override def condition(pa: Map[Int, Int]): LScheme = pa.get(variable) match {
    case Some(varAssignment) => DDis(variable,Map(varAssignment -> assignments(varAssignment).condition(pa)))
    case None => copy(assignments = assignments.map{case (k,v) => k -> v.condition(pa)})
  }
}
