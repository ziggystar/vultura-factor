package vultura.factor

import vultura.factor.generation.graph._
import vultura.util.SIIndex

import scala.language.postfixOps

package object generation {
  /** Generate lattice problems with fixed domain size for all variables, with parameters according to the Potts model.
    * @param dimensions The sizes of the dimensions of the grid. Set boolean indicator to true for wrapping dimension.
    */
  def pottsGrid(dimensions: Seq[(Int,Boolean)],
                domainSize: Int = 2,
                pairwisePotts: Generator[Double]): Generator[LabeledProblem[IndexedSeq[Int]]] =
    addPottsFactors(lattice(dimensions:_*), domainSize, _ => pairwisePotts)

  /** With constant domain size. */
  def addPottsFactors[A](graph: Graph[A], domainSize: Int, pairwisePotts: Set[A] => Generator[Double]): Generator[LabeledProblem[A]] = {
    val domains = Array.fill(graph.nodes.size)(domainSize)
    val varLabels = new SIIndex(graph.nodes)
    val factors: Generator[Seq[Factor]] = Generator.seq(graph.edges.toSeq.map(scope => pottsFactor(domains, scope.toSeq.map(varLabels.forward)(collection.breakOut), pairwisePotts(scope))))
    factors.map(fs => LabeledProblem(Problem(fs.toIndexedSeq, domains, LogD), varLabels))
  }

  /** Generates a Potts factor in Log domain. */
  def pottsFactor(domains: Array[Int], scope: Array[Var], coupling: Generator[Double]): Generator[Factor] = Generator(random =>
    Factor.fromFunction(scope, domains, state => if (state.distinct.size == 1) coupling.generate(random) else 0d)
  )

  /** Works only for binary valued problems.
    * For each variable, the energy generated by `energyOfOne` is added if the variable is 1. */
  def withMagneticField[A](p: LabeledProblem[A], energyOfOne: Generator[Double]): Generator[LabeledProblem[A]] = {
    require(p.problem.domains.forall(_ == 2))
    require(p.problem.ring == LogD)
    val singletons = p.problem.variables.map(v => energyOfOne.map(e => Factor(Array(v), Array(e, LogD.one))))
    Generator.seq(singletons).map( ss =>
      p.copy(problem = p.problem.copy(factors = p.problem.factors ++ ss))
    )
  }
  
  def problemGenerator[N](graph: Generator[Graph[N]], 
                          domains: Domainification[N] = FixedDomainsSize(2),
                          param: Parameterization[N] = IIDValuedParam(Generator.uniform(-1,1))): Generator[LabeledProblem[N]] =
    for{
      g <- graph
      ps <- domains.addDomains(g)
      p <- param.generateFactors(ps)
    } yield p
}
