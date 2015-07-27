package vultura.factor

import vultura.util.{SIIndex, IntDomainCPI}
import scala.language.postfixOps

/**
 * Created by thomas on 19.07.15.
 */
package object generation {
  /** Generate binary grid-networks with fixed domain size for all variables, with parameters according to the Potts model.
    * @param dimensions The sizes of the dimensions of the grid. Set boolean indicator to true for wrapping dimension.
    */
  def pottsBinaryGrid(dimensions: Iterable[(Int,Boolean)],
                      domainSize: Int = 2,
                      pairwisePotts: Generator[Double]): Generator[LabeledProblem[IndexedSeq[Int]]] = Generator{ r =>
    val structure = generateGrid(dimensions.toIndexedSeq, domainSize)
    val pairwiseFactors = structure.structure.scopeOfFactor.map(scope => Factor.fromFunction(scope, structure.structure.domains, state =>
      if(state.distinct.size == 1) pairwisePotts.generate(r) else 0d
    ))
    LabeledProblem(Problem(pairwiseFactors, structure.structure.domains, LogD), structure.variableLabels)
  }

  def withMagneticField[A](p: LabeledProblem[A], fieldGenerator: Generator[Double]): LabeledProblem[A] = {
    val singletons = p.problem.variables.map(v => Factor.)
    p.copy(problem = p.problem.copy(factors = p.problem.factors ++ ???))
  }

  def generateGrid(dimensions: IndexedSeq[(Int,Boolean)], domainSize: Int = 2): LabeledProblemStructure[IndexedSeq[Int]] = {
    val variables = new SIIndex[IndexedSeq[Int]](IntDomainCPI(dimensions.map(_._1).map(0 until).map(_.toArray).toArray).seq.map(_.toIndexedSeq))
    def nodeValid(coords: IndexedSeq[Int]): Option[IndexedSeq[Int]] = {
      val fixed = coords.zip(dimensions).map {
        case (xi, (di, _)) if xi >= 0 && xi < di => Some(xi)
        case (xi, (di, true)) if xi == -1 => Some(di - 1)
        case (xi, (di, true)) if xi == di => Some(0)
        case _ => None
      }
      val filtered = fixed.flatten
      if(filtered.length == fixed.length) Some(filtered)
      else None
    }
    val edges = for {
      node <- variables.elements
      shift <- IntDomainCPI(Array.fill(dimensions.size)(Array(0,1))) //only translate in positive directions to avoid double counting
      neighbour <- nodeValid(node.zip(shift).map(xy => xy._1 + xy._2)) if neighbour != node
    } yield (node,neighbour)
    LabeledProblemStructure(
      StructureOnly(
        variables.elements.map(_ => domainSize)(collection.breakOut),
        edges.map{case (i,j) => Array(i,j).map(variables.forward)(collection.breakOut): Array[Int]}(collection.breakOut)
      ), variables)
  }
}
