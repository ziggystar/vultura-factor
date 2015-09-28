package vultura.factor.generation

import vultura.factor._
import vultura.factor.generation.graph.Graph
import vultura.util.{Index, SIIndex}

import scala.util.Random

case class LabeledProblemStructure[N](structure: ProblemStructure, variableLabels: Index[N]){
  require(variableLabels.size == structure.numVariables)
}

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
