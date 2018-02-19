package vultura.factor.generation

import vultura.factor.{Factor, LabeledProblem, LogD}

@deprecated("use SingletonParameterization", "0")
/** A generator for singleton potentials. */
trait Magnetization[-N] {
  def magnetize[L <: N](p: LabeledProblem[L]): Generator[LabeledProblem[L]]
}
/** A generator for problem parameters that can be fit on a problem structure. */
trait Parameterization[-N]{
  def cliqueEnergy[L <: N](ps: LabeledProblemStructure[L]): ps.structure.FI => Generator[Array[Double]]
  final def parameterize[L <: N](ps: LabeledProblemStructure[L]): Generator[LabeledProblem[L]] = Generator { r =>
    val params: Map[ps.structure.FI,Array[Double]] = ps.structure.factorIndices.map(fi => fi -> cliqueEnergy(ps)(fi).generate(r))(collection.breakOut)
    ps.parameterize(params)
  }
}

/** Allows the provision of the parameterization via a function working on the labels. */
trait LabeledEnergyGenerator[-N]{ self: Parameterization[N] =>
  def cliqueEnergy[L <: N](ps: LabeledProblemStructure[L], domains: L => Int): IndexedSeq[L] => Generator[Array[Double]]

  final override def cliqueEnergy[L <: N](ps: LabeledProblemStructure[L]): (ps.structure.FI) => Generator[Array[Double]] = (fi: ps.structure.FI) =>
    cliqueEnergy(ps, ps.structure.domains compose ps.variableLabels.forward)(
      ps.structure.scopeOfFactor(fi).map(ps.variableLabels.backward)(collection.breakOut)
    )
}

trait SparseParameterization[-N] extends LabeledEnergyGenerator[N]{ self: Parameterization[N] =>
  def sparseCliqueEnergy[L <: N](ps: LabeledProblemStructure[L], domains: L => Int): IndexedSeq[L] => Option[Generator[Array[Double]]]
  final override def cliqueEnergy[L <: N](ps: LabeledProblemStructure[L], domains: (L) => Int): (IndexedSeq[L]) => Generator[Array[Double]] =
    (nodes: IndexedSeq[L]) => sparseCliqueEnergy(ps, domains)(nodes).getOrElse(Generator.only(Array.fill(nodes.map(domains).product)(0d)))
}

/** Adds the magnetization to the smallest clique that contains a variable. If there are multiple smallest cliques,
  * the one with the lexicographically smallest scope is chosen (using `implicitly[Ordering[Iterable[Int]]]` on the scope).
  * Note that this will not add factors to the structure.
  * If a variable does not appear in any factor, it will not be magnetized. */
case class SingletonParameterization[-N](magnetization: (N,Int) => Generator[Array[Double]]) extends Parameterization[N] {
  override def cliqueEnergy[L <: N](ps: LabeledProblemStructure[L]): ps.structure.FI => Generator[Array[Double]] = {
    type FI = ps.structure.FI
    type VI = ps.structure.VI
          val cliqueForVariable: Map[VI,FI] = ps.structure.variables.map{
                      vi => vi -> ps.structure.factorIdxOfVariable(vi)
                              .minBy(fi => ps.structure.scopeOfFactor(fi).length -> ps.structure.scopeOfFactor(fi).toIterable)
                    }(collection.breakOut)
    val variablesForFactor: Map[FI,Set[VI]] = cliqueForVariable.groupBy(_._2).mapValues(_.keySet).withDefaultValue(Set())
    (fi: ps.structure.FI) => Generator{ random =>
      Factor.multiply(LogD)(ps.structure.domains)(ps.structure.scopeOfFactor(fi).map{v =>
        if (variablesForFactor(fi).contains(v))
          Factor(Array(v), magnetization(ps.variableLabels.backward(v), ps.structure.domains(v)).generate(random))
        else
          Factor(Array(v),Array.fill(ps.structure.domains(v))(0d))
      }).values
    }
  }
}

object Parameterization {
  object zero extends Parameterization[Any]{
    override def cliqueEnergy[L <: Any](ps: LabeledProblemStructure[L]): (ps.structure.FI) => Generator[Array[Double]] =
      (fi: ps.structure.FI) => Generator.only(Array.fill(ps.structure.scopeOfFactor(fi).map(ps.structure.domains).product)(0d))
  }
  def add[U](p1: Parameterization[U], p2: Parameterization[U]): Parameterization[U] = new Parameterization[U] {
    override def cliqueEnergy[L <: U](ps: LabeledProblemStructure[L]): (ps.structure.FI) => Generator[Array[Double]] =
      (fi: ps.structure.FI) =>
        for {
          g1 <- p1.cliqueEnergy(ps)(fi)
          g2 <- p2.cliqueEnergy(ps)(fi)
        } yield (g1 zip g2).map{case (d1,d2) => d1 + d2}
  }
}

/** Each factor value is distributed iid within the logarithmic domain.
  * This means the generated value gets exponentiated before multiplication.
  * This is equivalent to an energy value for a Boltzmann distribution.
  * @param valueGen Generate values from the real-number line (this means negative values are ok).
  */
case class IIDValuedParam(valueGen: Generator[Double]) extends Parameterization[Any] with Magnetization[Any] {

  override def magnetize[L <: Any](p: LabeledProblem[L]): Generator[LabeledProblem[L]] = Generator{ random =>
    val transformation = if(p.problem.ring == LogD) identity[Double] _ else math.exp _

    def generateSingletonFactor(variable: Int): Factor =
      Factor.fromFunction(Array(variable), p.problem.domains, _ => transformation(valueGen.generate(random)))

    p.copy(problem = p.problem.copy(factors = p.problem.factors ++ p.problem.variables.map(generateSingletonFactor)))
  }

  override def cliqueEnergy[L <: Any](ps: LabeledProblemStructure[L]): (ps.structure.FI) => Generator[Array[Double]] = (fi: ps.structure.FI) =>
    Generator{r => Array.fill(ps.structure.scopeOfFactor(fi).map(ps.structure.domains).product)(valueGen.generate(r))}
}