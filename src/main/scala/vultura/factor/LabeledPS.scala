package vultura.factor

import vultura.util.SIIndex
import vultura.util.graph.DotGraph

import scala.util.Random

/** This class provides a variable-, and factor labeled problem structure, that can be used to
  * generate random instances. */
case class LabeledPS[V,F](variables: Set[V] = Set(), factorScopes: Map[F,Set[V]] = Map()){
  require(factorScopes.values.flatten.toSet.subsetOf(variables))
  val variableIndex: SIIndex[V] = new SIIndex(variables)
  val factorIndex: SIIndex[F] = new SIIndex(factorScopes.keySet)
  def addVariables(vs: V*): LabeledPS[V, F] =
    copy(variables = variables ++ vs)
  def addFactors(fs: (F,Set[V])*): LabeledPS[V, F] =
    copy(variables = variables ++ fs.flatMap(_._2), factorScopes = factorScopes ++ fs)

  def incidenceGraph: DotGraph[V] = {
    def allUnorderedPairs[A](xs: Seq[A], acc: Seq[(A,A)] = Seq()): Seq[(A,A)] =
      if(xs.isEmpty) acc else allUnorderedPairs(xs.tail, xs.tail.map(xs.head -> _) ++ acc)
    DotGraph(factorScopes.toSeq.flatMap(f => allUnorderedPairs(f._2.toSeq)),additionalNodes = variables)
      .nodeLabeled{
      case (x,y,0) => s"$x,$y"
      case (x,y,1) => s"h$x,$y"
      case x       => s"unknown node $x"
    }
  }

  def factorGraph: DotGraph[Either[V,F]] =
    DotGraph(factorScopes.toSeq.flatMap{case (f,vs) => vs.map(v => Right(f) -> Left(v))})

  def generateProblem(potGen: F => FactorGenerator, domainSize: V => Int, random: Random = new Random(0)): Problem = {
    val domains: Array[Int] = variableIndex.elements.map(domainSize)(collection.breakOut)
    val factors = factorIndex.elements.map { f =>
      val fvars = factorScopes(f).map(variableIndex.forward).toArray.sorted
      potGen(f).generate(fvars,domains,random)
    }
    Problem(factors, domains, NormalD)
  }
}
object LabeledPS {
  def empty[V,F] = LabeledPS[V,F](Set(),Map())
}