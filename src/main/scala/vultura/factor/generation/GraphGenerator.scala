package vultura.factor.generation

import vultura.factor._
import vultura.util.Index

case class LabeledProblemStructure[N](structure: ProblemStructure, variableLabels: Index[N]){
  require(variableLabels.size == structure.numVariables)
}

case class LabeledProblem[N](problem: Problem, variableLabels: Index[N])
