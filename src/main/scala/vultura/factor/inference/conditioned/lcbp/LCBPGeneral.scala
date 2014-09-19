package vultura.factor.inference.conditioned.lcbp

import vultura.factor.inference.calibration._
import vultura.factor._
import vultura.factor.inference.{JunctionTree, ParFunI, MargParI, JointMargI}
import vultura.util.{SSet, ArrayIndex}

import scala.runtime.ObjectRef

/** LCBP inference where inference on the meta problem can be any inference algorithm.
 */
case class LCBPGeneral(scheme: FactoredScheme,
                       inferer: Problem => MargParI with JointMargI = p => new JunctionTree(p),
                       maxUpdates: Long = 100000,
                       tol: Double = 1e-9) extends LcbpBase with ParFunI {
  //the scheme type we require
  override type ST = FactoredScheme

  val metaRing = LogD

  /** Maps the index of the conditioners in the original problem to a new index in the meta problem. */
  val mcVariablesIdx = new ArrayIndex[Int](scheme.allConditioners.toSeq.sorted)

  val mcDomains: Array[Int] = mcVariablesIdx.elements.map(problem.domains)(collection.breakOut)

  sealed trait EnergyContrib {
    /** The index of the influencing conditioners within the original problem. */
    def conditioners: Array[Int]
    lazy val mpVariables: Array[Int] = conditioners.map(mcVariablesIdx.forward)
    def isConstant = conditioners.isEmpty
    def sourceEdge(c: C): DoubleEdge
  }
  //use early initializer for the construction of `mpvariables` in `EnergyContrib`
  case class VariableContribution(vi: Int) extends {
    val conditioners: Array[Int] = scheme.conditioners(vi).toArray
  } with EnergyContrib {
    override def sourceEdge(c: C): DoubleEdge = {
      require(c.keySet == conditioners.toSet)
      FVariable(vi,c)
    }
  }
  case class FactorContribution(fi: Int) extends {
    val conditioners: Array[Int] =
      scheme.conditionersOf(problem.factors(fi).variables.toSet).toArray
  } with EnergyContrib {

    override def sourceEdge(c: C): DoubleEdge = {
      require(c.keySet == conditioners.toSet)
      FFactor(fi,c)
    }
  }

  val contributions: IndexedSeq[EnergyContrib] =
    (problem.variables.map(VariableContribution) ++ (0 until problem.factors.size).map(FactorContribution))

  private val ssetOfMPCliques: SSet[Int] = new SSet(contributions.map(_.mpVariables.toSet).toSet)

  val contributionValues: IndexedSeq[(EnergyContrib,C)] = for{
    contrib <- contributions
    condition <- scheme.allAssignmentsTo(contrib.conditioners.toSet)
  } yield (contrib,condition)

  def buildProblem(energyValues: IndexedSeq[Double]): Problem = {
    val lookup: Map[(EnergyContrib, C), Double] = contributionValues.zip(energyValues).toMap
    def buildFactor(contrib: EnergyContrib): Factor = Factor.fromFunction(
      contrib.mpVariables,
      mcDomains,
      cond => lookup((contrib, contrib.conditioners.zip(cond).toMap))
    )

    Problem(
      factors = contributions.map(buildFactor),
      domains = mcDomains,
      ring = metaRing
    ).simplify
  }

  case object MetaProblem extends LcbpMessage {
    override type InEdge = DoubleEdge
    override type TOut = ObjectRef[MargParI with JointMargI]

    override def copy(t: TOut): TOut = new ObjectRef(t.elem)

    /** Only has to create the value container (e.g. an array); the value will be taken from elsewhere. */
    override def create: TOut = new ObjectRef(inferer(buildProblem(IndexedSeq.fill(inputs.size)(metaRing.one))))

    //input are all variable and factor contributions to the free energy
    override val inputs: IndexedSeq[DoubleEdge] = contributionValues.map{case (contr,c) => contr.sourceEdge(c)}

    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit = { (energyValues,problemRef) =>
      val problem: Problem = buildProblem(energyValues.map(_.value))
      problemRef.elem = inferer(problem)
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
    val clique: Array[Int] = ssetOfMPCliques.maximalSuperSetsOf(fc.keySet.map(mcVariablesIdx.forward)).head.toArray.sorted
    val fcVariables: Array[Int] = fc.keySet.toArray.sorted
    val fcCondition: Array[Val] = fcVariables.map(fc)
    //an ordering of the conditioners in vc as variables of the meta problem
    val fcVariablesMP: Array[Int] = fcVariables.map(mcVariablesIdx.forward)
    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit = { (ins,resultRef) =>
      val infResult: MargParI with JointMargI = ins(0
      ).elem
      val belief = infResult.decodedCliqueBelief(clique)
      val summed = Factor.multiplyRetain(NormalD)(mcDomains)(Seq(belief),fcVariablesMP)
      resultRef.value = summed.eval(fcCondition, mcDomains)
    }
  }

  /** This must return a double-valued edge that computes the probability of condition `fc` given condition `vc`. */
  override def ccp(fc: C, vc: C): DoubleEdge = CCP(fc,vc)

  val cp: Set[LcbpMessage] = Edge.expand(cdLogZ)

  object initializer extends EdgeValues[LcbpMessage]{
    override def hasEdge(e: LcbpMessage): Boolean = true
    override def edgeValue(e: LcbpMessage): e.type#TOut = e match {
      case FactorEdge(vars) => Factor.maxEntropy(vars,problem.domains,problem.ring).values.asInstanceOf[e.TOut]
      case ve: DoubleEdge => new DoubleRef(problem.ring.one).asInstanceOf[e.TOut]
      case mp: MetaProblem.type => e.create
    }
  }

  object convTest extends ConvergenceTest[LcbpMessage] {
    def isConverged(e: LcbpMessage)(old: e.type#TOut, updated: e.type#TOut): Boolean = ((old,updated) match {
      case (o: Array[Double], u: Array[Double]) => vultura.util.maxDiff(o,u)
      case (o: DoubleRef, u: DoubleRef) => math.abs(o.value - u.value)
      case (o: ObjectRef[MetaProblem.type], u: ObjectRef[MetaProblem.type]) => Double.PositiveInfinity //hope this works
    }) <= tol
  }

  val calibrator: MutableFIFOCalibrator[LcbpMessage] = new MutableFIFOCalibrator[LcbpMessage](cp)(
    convTest,
    maxUpdates,
    initializer)

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(calibrator.edgeValue(cdLogZ).value)

  /** @return Natural logarithm of partition function. */
  override def logZ: Double = calibrator.edgeValue(cdLogZ).value
}
