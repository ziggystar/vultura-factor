package vultura.factor

import vultura.util.TreeWidth

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

  def isTree: Boolean = ???
}

/** Implementation of [[ProblemStructure]]. */
case class StructureOnly(domains: Array[Int], scopeOfFactor: Array[Array[Int]]) extends ProblemStructure {
  override def equals(obj: scala.Any): Boolean = obj match {
    case ps: ProblemStructure => domains.deep == ps.domains.deep && scopeOfFactor.deep == ps.scopeOfFactor.deep
  }
  override val hashCode: Int = (domains.deep,scopeOfFactor.deep).hashCode
}

