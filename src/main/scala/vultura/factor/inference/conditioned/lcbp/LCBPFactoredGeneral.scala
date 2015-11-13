package vultura.factor.inference.conditioned.lcbp

import vultura.factor._
import vultura.factor.inference.calibration.{MutableFIFOCalibrator, ConvergenceTest, EdgeValues, Edge}
import vultura.factor.inference.conditioned._
import vultura.factor.inference.{JointMargI, ParFunI, JunctionTree, MargParI}

import scala.runtime.ObjectRef

/** LCBP inference where inference on the meta problem can be any inference algorithm.
  */
case class LCBPFactoredGeneral(scheme: FactoredScheme,
                               inferer: Problem => JointMargI with ParFunI = p => new JunctionTree(p),
                               maxUpdates: Long = 100000,
                               tol: Double = 1e-9,
                               useDeltaTerm: Boolean = false) extends LcbpFactoredBase with MargParI {
  override type ST = FactoredScheme

  case object MetaProblem extends LcbpMessage {
    override type InEdge = MetaFactor
    override type TOut = ObjectRef[JointMargI with ParFunI]

    override def copy(t: TOut): TOut = new ObjectRef(t.elem)

    //input are all variable and factor contributions to the free energy
    override val inputs: IndexedSeq[MetaFactor] = metaStructure.factorIndices.map(MetaFactor)

    /** Only has to create the value container (e.g. an array); the value will be taken from elsewhere. */
    override def create: TOut = new ObjectRef(inferer(
      buildProblem(metaStructure.scopeOfFactor.map(scope => Factor.maxEntropy(scope,metaStructure.domains,metaRing).values))
    ))

    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[Array[Double]], TOut) => Unit = { (factorValues,problemRef) =>
      val problem: Problem = buildProblem(factorValues)
      problemRef.elem = inferer(problem)
    }

    def buildProblem(factorValues: IndexedSeq[Array[Double]]): Problem = {
      Problem(
        factorValues.zip(metaStructure.scopeOfFactor).map { case (vals, vars) => Factor(vars, vals) },
        metaStructure.domains,
        metaRing)
    }
  }

  case object cdLogZ extends DoubleEdge {
    override type InEdge = MetaProblem.type
    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[MetaProblem.TOut], DoubleRef) => Unit = {
      (in,zref) => zref.value = in(0).elem.logZ
    }
    override def inputs: IndexedSeq[InEdge] = IndexedSeq(MetaProblem)
  }

  case class CCP(fc: C, vc: C) extends DoubleEdge{
    override type InEdge = MetaProblem.type
    override def inputs: IndexedSeq[InEdge] = IndexedSeq(MetaProblem)
    val clique: Array[Int] = metaConditionersOf(fc.keySet).toArray.sorted
    val fcVariables: Array[Int] = fc.keySet.toArray.sorted
    val fcCondition: Array[Val] = fcVariables.map(fc)
    //an ordering of the conditioners in vc as variables of the meta problem
    val fcVariablesMP: Array[Int] = fcVariables.map(var2metaVar.forward)
    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit = { (ins,resultRef) =>
      val infResult: JointMargI with ParFunI = ins(0).elem
      val belief = infResult.decodedCliqueBelief(clique)
      val summed = Factor.multiplyRetain(NormalD)(metaStructure.domains)(Seq(belief),fcVariablesMP)
      resultRef.value = summed.eval(fcCondition, metaStructure.domains)
    }
  }

  /** This must return a double-valued edge that computes the probability of condition `fc` given condition `vc`. */
  override def ccp(fc: C, vc: C): DoubleEdge = CCP(fc,vc)

  val cp: Set[LcbpMessage] = Edge.expand(cdLogZ)

  //stuff that should be factored out below

  object initializer extends EdgeValues[LcbpMessage]{
    override def hasEdge(e: LcbpMessage): Boolean = true
    override def edgeValue(e: LcbpMessage): e.type#TOut = e match {
      case FactorEdge(vars) => Factor.maxEntropy(vars,problem.domains,problem.ring).values.asInstanceOf[e.TOut]
      case CCP(fc,vc) =>  new DoubleRef(1d / scheme.subConditionsOf(vc, fc.keySet).size).asInstanceOf[e.TOut]
      case ve: DoubleEdge => new DoubleRef(problem.ring.one).asInstanceOf[e.TOut]
      case mp: MetaProblem.type => e.create
    }
  }

  object convTest extends ConvergenceTest[LcbpMessage] {
    def isConverged(e: LcbpMessage)(old: e.type#TOut, updated: e.type#TOut): Boolean = ((old,updated) match {
      case (o: Array[Double], u: Array[Double]) => vultura.util.maxDiff(o,u)
      case (o: DoubleRef, u: DoubleRef) => math.abs(o.value - u.value)
      case (o: ObjectRef[_], u: ObjectRef[_]) => Double.PositiveInfinity //hope this works
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
    val conditions: Seq[Condition] = scheme.conditionsOf(Set(vi)).toSeq
    val conditionClique = calibrator.edgeValue(MetaProblem).elem //get meta distribution
      .decodedCliqueBelief(scheme.conditionersOf(Set(vi)).map(var2metaVar.forward).toArray.sorted)
    val conditionWeights = conditions.map{c =>
      val mappedCond: Map[Var, Val] = c.map{case (k,v) => var2metaVar.forward(k) -> v}
      conditionClique.eval(conditionClique.variables.map(mappedCond),metaStructure.domains)
    }
    val conditionedVariableBeliefs: IndexedSeq[VBel#TOut] = conditions.map(c => calibrator.edgeValue(VBel(vi,c)))(collection.breakOut)
    val values = linearCombination(conditionWeights.toArray,conditionedVariableBeliefs)
    Factor(Array(vi),values)
  }
}