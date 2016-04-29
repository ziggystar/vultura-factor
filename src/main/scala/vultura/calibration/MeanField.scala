package vultura.calibration

import vultura.factor.inference.VariationalResult
import vultura.factor.{Ring, Factor, NormalD, Problem}

/** Mean field approximation. */
case class MeanField(p: Problem) extends CalProblem with ResultBuilder[VariationalResult] {
  require(p.ring == NormalD, "mean field only implemented for normal encoding")

  override type Parameter = Unit

  type N = VBel
  /* Parameters of fully factored mean field are the marginal distributions over the variables. */
  case class VBel(vi: p.VI) extends ComputedNode {
    /** Size of the array required to store the state of this edge. */
    override def arraySize: Int = p.domains(vi)
    override def dependencies: IndexedSeq[VBel] = p.neighboursOfVariableEx(vi).map(VBel)
    override def compute(ins: Array[IR], result: IR): Unit = {
      val incidentVariableBeliefs: IndexedSeq[Factor] =
        dependencies.zip(ins).map { case (VBel(vn), values) => Factor(Array(vn), values) }

      val localBelief = Factor.constant(Array(vi), p.domains, 1d)

      def beliefOf(v: Int): Factor = if(v == vi) localBelief else incidentVariableBeliefs(p.neighboursOfVariableEx(vi).indexOf(v))
      val factorContributions = p.factorsOfVariable(vi).map{f =>
        val cavity = Factor.multiply(NormalD)(p.domains)(f.variables.map(beliefOf))

        val logFactor = f.map(math.log)
        Factor.multiplyRetain(NormalD)(p.domains)(IndexedSeq(cavity,logFactor),Array(vi)).values
      }
      val summedEnergies: Array[Double] = factorContributions.transpose.map { energyTerms =>
        val sum = energyTerms.foldLeft(0d) { case (ac, n) => if (n.isNaN) Double.NegativeInfinity else ac + n }
        math.exp(sum)
      }
      System.arraycopy(NormalD.normalize(summedEnergies),0,result,0,result.length)
    }

    def init: Array[Double] = NormalD.normalize(Array.fill(arraySize)(1d))
  }

  /** Constructs a new initial value for each edge. */
  override def initializer(u: Unit): N => IR = e => e.init

  override def nodes: Set[N] = p.variables.map(VBel)(collection.breakOut)

  override def buildResult(valuation: (VBel) => IR): VariationalResult = new VariationalResult {
    override def ring: Ring[Double] = problem.ring

    lazy val averageEnergy: Double = p.factors.foldLeft(0d) { case (e, f) =>
      val factorBelief = Factor.multiply(NormalD)(p.domains)(f.variables.map(encodedVarBelief))
      val factorEnergy = NormalD.logExpectation(factorBelief.values,f.values)
      e + factorEnergy
    }

    lazy val entropy: Double =
      p.variables.foldLeft(0d){case (a,v) => a + NormalD.entropy(encodedVarBelief(v).values)}

    /** @return marginal distribution of variable in encoding specified by `ring`. */
    override def encodedVarBelief(variable: Int): Factor = Factor(Array(variable), valuation(VBel(variable)))

    override def problem: Problem = p
  }
}
