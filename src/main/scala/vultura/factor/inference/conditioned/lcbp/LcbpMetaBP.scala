package vultura.factor.inference.conditioned.lcbp

import vultura.factor._
import vultura.factor.inference.MargParI
import vultura.factor.inference.calibration._
import vultura.factor.inference.conditioned._

/** BP on the meta problem.
 */
class LcbpMetaBP(val scheme: FactoredScheme, val maxUpdates: Long = 1000000, val tol: Double = 1e-12, val useDeltaTerm: Boolean = true)
  extends LcbpFactoredBase with MargParI {

  override def ring: Ring[Double] = scheme.problem.ring

  override type ST = FactoredScheme

  case class MetaV2F(v: MVI, fi: MFI) extends MetaFactorEdge {
    val variables: Array[MVI] = Array(v)
    override type InEdge = MetaF2V
    override def inputs: IndexedSeq[InEdge] =
      for(nf <- metaStructure.factorIdxOfVariable(v) if nf != fi) yield MetaF2V(nf,v)
    def mCompute() = {
      //this must be lazy, otherwise inputs gets called indefinitely
      val spTask = SumProductTask(
        remainingVars = variables,
        domainSizes = metaStructure.domains,
        inputs.map(f2v => Array(f2v.v))(collection.breakOut),
        metaRing
      )
      (ins: IndexedSeq[Array[Double]], result: Array[Double]) => {
        spTask.sumProduct(ins,result)
        metaRing.normalizeInplace(result)
      }
    }
  }

  case class MetaF2V(fi: MFI, v: MVI) extends MetaFactorEdge {
    val variables: Array[MVI] = Array(v)
    override type InEdge = MetaFactorEdge
    //first input is the factor value, tail are the incoming messages (except the one from `v`)
    override def inputs: IndexedSeq[InEdge] =
      metaFactorEdge(fi) +: (for(nv <- metaStructure.scopeOfFactor(fi) if nv != v) yield MetaV2F(nv,fi))
    override def mCompute(): (IndexedSeq[Array[Double]], Array[Double]) => Unit =
      constructSPTask(inputs.map(_.variables),variables,Seq(),metaStructure.domains,metaRing)
  }

  case class MetaFBel(fi: MFI) extends MetaFactorEdge {
    val variables: Array[Int] = metaStructure.scopeOfFactor(fi)
    override type InEdge = MetaFactorEdge
    override def inputs: IndexedSeq[InEdge] =
      metaFactorEdge(fi) +: (for(mvi <- variables) yield MetaV2F(mvi,fi))
    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit =
      constructSPTask(inputs.map(_.variables), variables, Seq(), metaStructure.domains, metaRing)
  }

  case class MetaVBel(vi: MVI) extends MetaFactorEdge {
    override type InEdge = MetaF2V
    override def variables: Array[Int] = Array(vi)

    override def inputs: IndexedSeq[InEdge] =
      for(fi <- metaStructure.factorIdxOfVariable(vi)) yield MetaF2V(fi,vi)

    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[TOut], TOut) => Unit =
      constructSPTask(inputs.map(_.variables), variables, Seq(), metaStructure.domains, metaRing)
  }

  case class CCP(fc: C, vc: C) extends DoubleEdge {
    override type InEdge = MetaFBel
    val mfc: Condition = conditionToMetaCondition(fc)
    val mvc: Condition = conditionToMetaCondition(vc)
    assert(mvc.keySet.subsetOf(mfc.keySet))

    //the factor belief of the meta problem we require
    val edge: MetaFBel = MetaFBel(containingMetaClique(mfc.keys))
    //we marginalize and retain these variables
    val retainVars: Array[Int] = (mfc.keySet -- mvc.keySet).toArray.sorted
    //and that's the assignment we are interested in
    val conditionValues: Array[Int] = retainVars.map(mfc)

    override def inputs: IndexedSeq[InEdge] = IndexedSeq(edge)

    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], DoubleRef) => Unit = (fb,res) => {
      val rLog = Factor
        .multiplyRetain(metaRing)(metaStructure.domains)(Seq(edge.makeFactor(fb.head)), retainVars) //marginalize to mvc condition
        .eval(conditionValues, metaStructure.domains) //select proper value
      res.value = math.exp(rLog)
    }
  }

  /** This must return a double-valued edge that computes the partition function of the meta problem. */
  case object cdLogZ extends DoubleEdge {
    override type InEdge = MetaFactorEdge
    override def inputs: IndexedSeq[InEdge] =
      metaStructure.variables.map(MetaVBel) ++
        (0 until metaStructure.numFactors).map(MetaFBel) ++
        (0 until metaStructure.numFactors).map(MetaFactor)

    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit = { (ins, res) =>
      val (vbels,fbels,factors) = {
        val (vb,rest) = ins.splitAt(metaStructure.numVariables)
        val (fb,fs) = rest.splitAt(metaStructure.numFactors)
        (vb,fb,fs)
      }
      val variableEntropy = vbels.map(metaRing.entropy)
        .zip(metaStructure.variables.map(v => 1 - metaStructure.degrees(v)))
        .map{case (x,y) => x * y}.sum
      val factorEntropy = fbels.map(metaRing.entropy).sum
      val factorLogExpect = (fbels zip factors).map{case (fb,f) => metaRing.logExpectation(fb,f)}.sum
      res.value = variableEntropy + factorEntropy + factorLogExpect
    }
  }

  /** This must return a double-valued edge that computes the probability of condition `fc` given condition `vc`. */
  override def ccp(fc: C, vc: C): DoubleEdge = CCP(fc,vc)

  val cp: Set[LcbpMessage] = Edge.expand(cdLogZ)

  object initializer extends EdgeValues[LcbpMessage]{
    override def hasEdge(e: LcbpMessage): Boolean = true
    override def edgeValue(e: LcbpMessage): e.type#TOut = e match {
      case FactorEdge(vars) => Factor.maxEntropy(vars,problem.domains,problem.ring).values.asInstanceOf[e.TOut]
      case CCP(fc,vc) =>  new DoubleRef(1d / scheme.subConditionsOf(vc, fc.keySet).size).asInstanceOf[e.TOut]
      case ve: DoubleEdge => new DoubleRef(problem.ring.one).asInstanceOf[e.TOut]
    }
  }

  object convTest extends ConvergenceTest[LcbpMessage] {
    def isConverged(e: LcbpMessage)(old: e.type#TOut, updated: e.type#TOut): Boolean = ((old,updated) match {
      case (o: Array[Double], u: Array[Double]) => vultura.util.maxDiff(o,u)
      case (o: DoubleRef, u: DoubleRef) => math.abs(o.value - u.value)
      case _ => throw new MatchError("shouldn't happen")
    }) <= tol
  }

  val calibrator: MutableFIFOCalibrator[LcbpMessage] = new MutableFIFOCalibrator[LcbpMessage](cp)(
    convTest,
    maxUpdates,
    initializer)

  /** @return Natural logarithm of partition function. */
  override def logZ: Double = calibrator.edgeValue(cdLogZ).value

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def encodedVarBelief(vi: Var): Factor = {
    val conditions: Array[Condition] = scheme.conditionsOf(Set(vi)).toArray
    val metaScope_x: Set[MFI] = scheme.conditionersOf(Set(vi)).map(var2metaVar(_))
    //find the correct meta factor (it has to exist)
    val (metaScope,mfi) = metaScopes.zipWithIndex.find{case (msc,_) => msc.toSet == metaScope_x}.get
    val mfactorBel = Factor(metaScope, calibrator.edgeValue(MetaFBel(mfi)))
    val conditionWeights: Array[Double] = conditions.map{c =>
      if(c.isEmpty) metaRing.one else {
        val mappedCond: Map[Var, Val] = c.map { case (k, v) => var2metaVar(k) -> v}
        val values: Array[Val] = metaScope.map(mappedCond)
        mfactorBel.eval(values, metaStructure.domains)
      }
    }
    val conditionedVariableBeliefs: IndexedSeq[VBel#TOut] = conditions.map(c => calibrator.edgeValue(VBel(vi,c)))(collection.breakOut)
    val values = linearCombination(metaRing.decode(conditionWeights),conditionedVariableBeliefs)
    Factor(Array(vi),values)
  }
}
