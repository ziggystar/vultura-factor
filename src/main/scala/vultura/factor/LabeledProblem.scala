package vultura.factor

import vultura.util.Index

case class LabeledProblem[N](problem: Problem, variableLabels: Index[N])
