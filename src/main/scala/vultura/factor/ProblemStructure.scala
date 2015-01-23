package vultura.factor

import scala.annotation.tailrec

/** Describes the structure of a markov network, without the factor values (parameters). */
trait ProblemStructure {
  type VI = Int
  type FI = Int
  /** The domain size for each variable. Variable indices start at zero and are consecutive. */
  def domains: Array[Int]
  def scopeOfFactor: Array[Array[VI]]

  val numVariables: Int = domains.size
  val numFactors: Int = scopeOfFactor.size

  def variables: Range = 0 until numVariables
  lazy val variableSet: Set[Int] = (0 until numVariables).toSet

  lazy val degrees: Array[Int] = {
    val r = new Array[Int](numVariables)
    for{
      scope <- scopeOfFactor
      vi <- scope
    } {
      r(vi) = r(vi) + 1
    }
    r
  }

  /** Indexing with a variable index yields the factor indices this variable participates in. */
  lazy val factorIdxOfVariable: Array[Array[FI]] = {
    val result: Array[Array[Int]] = degrees.map(new Array[Int](_))
    val counter = new Array[Int](numVariables)
    var fi = 0
    while(fi < numFactors){
      val scope = scopeOfFactor(fi)
      var vi = 0
      while(vi < scope.size){
        val v = scope(vi)
        result(v)(counter(v)) = fi
        counter(v) = counter(v) + 1
        vi = vi + 1
      }
      fi = fi + 1
    }
    result
  }
  /** The index with variable index `vi`, yields the neighbours of the variable, without `vi`. */
  lazy val neighboursOfVariableEx: Array[Array[VI]] =
    factorIdxOfVariable.zipWithIndex.map{ case (fs,v) => fs.flatMap(fi => scopeOfFactor(fi)).filterNot(_ == v) }
  lazy val neighboursOfVariableInc: Array[Array[VI]] =
    factorIdxOfVariable.zipWithIndex.map{ case (fs,v) => fs.flatMap(fi => scopeOfFactor(fi)) }

  def isTree: Boolean = {
    @tailrec
    def findCycle(remaining: List[Set[Int]] = scopeOfFactor.map(_.toSet)(collection.breakOut),
                  closed: Set[Int] = Set()): Boolean = remaining match {
      case Nil => true
      case n :: tail if n.isEmpty => findCycle(tail,closed)
      case n :: _ =>
        val elim = n.head
        val (x,y) = remaining.partition(_.contains(elim))
        val flatX: Set[Int] = (x.flatMap(identity)(collection.breakOut): Set[Int]) - elim
        flatX.size == (x.map(_.size).sum - x.size) && findCycle(flatX :: y, closed + elim)
    }
    findCycle()
  }
}

/** Implementation of [[ProblemStructure]]. */
case class StructureOnly(domains: Array[Int], scopeOfFactor: Array[Array[Int]]) extends ProblemStructure {
  override def equals(obj: scala.Any): Boolean = obj match {
    case ps: ProblemStructure => domains.deep == ps.domains.deep && scopeOfFactor.deep == ps.scopeOfFactor.deep
  }
  override val hashCode: Int = (domains.deep,scopeOfFactor.deep).hashCode
}

