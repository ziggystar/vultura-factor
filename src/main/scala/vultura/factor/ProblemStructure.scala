package vultura.factor

import vultura.util.graph.{BiSetGraph, DotGraph, undirected}

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
  def factorIndices: Range = 0 until numFactors

  lazy val variableSet: Set[Int] = (0 until numVariables).toSet

  /** Number of incident factors to variable. Indexed by variable index. */
  lazy val factorDegreeOfVariable: Array[Int] = {
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
    val result: Array[Array[Int]] = factorDegreeOfVariable.map(new Array[Int](_))
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
  /** The index with variable index `vi`, yields the variable nodes that are neighbours of the
    * variable `vi` in the Markov network, excluding `vi` itself. */
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

  def isCompatible(other: ProblemStructure): Boolean =
    domains.deep == other.domains.deep && scopeOfFactor.deep == other.scopeOfFactor.deep

  def withParameters(parameters: FI => Array[Double], ring: Ring[Double]): Problem =
    Problem(
      scopeOfFactor.zip(factorIndices map parameters).map{case (scope, params) => Factor(scope,params)}(collection.breakOut),
      domains,
      ring)

  def dotFactorGraph: DotGraph[Either[VI,FI]] =
    DotGraph(for{f <- factorIndices; v <- scopeOfFactor(f)} yield (Left(v),Right(f)), variables.map(Left(_)))
  def dotMarkovNetwork: DotGraph[VI] =
    DotGraph((for{fs <- scopeOfFactor; v1 <- fs; v2 <- fs if v1 < v2} yield (v1,v2)).toSet,variables)

  def markovNetwork: BiSetGraph[VI] =
    undirected.fromTuples(neighboursOfVariableEx.zipWithIndex.flatMap{case (ns,i) => ns.map(_ -> i)}, extraNodes = variableSet)

  /** Generate a problem by supplying a parameterization. */
  def parameterize(parameterization: FI => Array[Double], ring: Ring[Double] = LogD): Problem =
    Problem(factors = factorIndices.map(fi => Factor(scopeOfFactor(fi),parameterization(fi))), domains, ring)
}

