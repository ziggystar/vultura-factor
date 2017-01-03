package vultura.factor.generation

import vultura.factor._
import vultura.factor.generation.graph.Graph
import vultura.util.{Index, SIIndex}

import scala.collection.immutable.Seq

case class LabeledProblemStructure[N](structure: ProblemStructure, variableLabels: Index[N]){
  require(variableLabels.size == structure.numVariables)
  def parameterize(params: structure.FI => Array[Double], ring: Ring[Double] = LogD): LabeledProblem[N] =
    LabeledProblem(structure.parameterize(params),variableLabels)
  def addSingletons(pred: N => Boolean): LabeledProblemStructure[N] = {
    val singletonsToAdd: Seq[Var] = structure.variables
      .filter(pred compose variableLabels.backward)
        .filter(vi => structure.factorIdxOfVariable(vi).forall(fi => structure.scopeOfFactor(fi).length != 1))
    LabeledProblemStructure(StructureOnly(structure.domains,structure.scopeOfFactor ++ singletonsToAdd.map(Array(_))), variableLabels)
  }
}

object LabeledProblemStructure {
  def fromGraph[N](g: Graph[N], domainSize: N => Int): LabeledProblemStructure[N] = {
    val nodes = new SIIndex(g.nodes)
    val domains: Array[Int] = nodes.elements.map(domainSize)(collection.breakOut)
    LabeledProblemStructure(StructureOnly(domains, g.edges.map(_.map(nodes.forward)(collection.breakOut): Array[Int])(collection.breakOut)), nodes)
  }
}

case class LabeledProblem[N](problem: Problem, variableLabels: Index[N])
