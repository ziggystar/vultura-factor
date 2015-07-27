package vultura.factor.generation

import vultura.factor._
import vultura.util.{Index, SIIndex}

import scala.annotation.tailrec
import scala.collection.TraversableOnce.MonadOps
import scala.util.Random

/** A Hypergraph without parallel edges. */
case class Graph[N](nodes: Set[N], edges: Set[Set[N]]){
  def filterNodes(p: N => Boolean): Graph[N] = Graph(nodes filter p,edges filter (_.forall(p)))
  def filterEdges(p: Set[N] => Boolean): Graph[N] = Graph(nodes, edges filter p)
  /** Adds a new edge to the graph, if it covers a new vertex, it will be silently added. */
  def addEdge(e: Set[N]): Graph[N] = Graph(nodes ++ e, edges + e)
}

case class LabeledProblemStructure[N](structure: ProblemStructure, variableLabels: Index[N]){
  require(variableLabels.size == structure.numVariables)
}

/** Basically a distribution over values of type `A`. */
trait Generator[A] { outer =>
  def generate(r: Random): A
  def map[B](f: A => B): Generator[B] = new Generator[B]{
    override def generate(r: Random): B = f(outer.generate(r))
  }
  def flatMap[B](f: A => Generator[B]): Generator[B] = new Generator[B]{
    override def generate(r: Random): B = f(outer.generate(r)).generate(r)
  }
  def withFilter(p: A => Boolean): Generator[A] = new Generator[A] {
    def generate(r: Random): A = {
      var res: A = null.asInstanceOf[A]
      do {
        res = outer.generate(r)
      } while(!p(res))
      res
    }
  }
}

object Generator {
  def apply[A](f: Random => A): Generator[A] = new Generator[A]{
    override def generate(r: Random): A = f(r)
  }
}

case class Constant[A](a: A) extends Generator[A]{
  override def generate(r: Random): A = a
}
/** Generates a node-labelled hyper graph structure. */
trait GraphGenerator[N] extends Generator[Graph[N]]


/** Attaches domain sizes to the nodes of a hyper graph. */
case class StructureGenerator[N](graphGen: Generator[Graph[N]], domainGenerator: N => Generator[Int])
  extends Generator[LabeledProblemStructure[N]] {

  override def generate(r: Random): LabeledProblemStructure[N] = {
    val graph = graphGen.generate(r)
    val nodes = new SIIndex(graph.nodes)
    val domains: Array[Int] = nodes.elements.map(domainGenerator).map(_.generate(r))(collection.breakOut)
    val structure = StructureOnly(domains,graph.edges.map(_.map(nodes.forward)(collection.breakOut): Array[Int])(collection.breakOut))
    LabeledProblemStructure(structure,nodes)
  }
}

object StructureGenerator {
  def fixedDomainSize[N](graphGen: Generator[Graph[N]], domainSize: Int): StructureGenerator[N] =
    StructureGenerator(graphGen,_ => Constant(domainSize))
}

case class LabeledProblem[N](problem: Problem, variableLabels: Index[N])


case class JointProblemGenerator[N](structure: Generator[LabeledProblemStructure[N]],
                                    factorGenerator: FactorGenerator[N])
  extends Generator[(Problem,Index[N])]{
  override def generate(r: Random): LabeledProblem[N] = {
    val str = structure.generate(r)
    val p = Problem(
      str.structure.scopeOfFactor.map { scope =>
        factorGenerator.generateFactor(scope map str.structure.domains,scope.map(str.variableLabels.backward)(collection.breakOut),r)
      }(collection.breakOut),
      str.structure.domains,
      factorGenerator.ring)
    LabeledProblem(p,str.variableLabels)
  }
}
