package vultura.factor.inference.conditioned.lcbp

import vultura.factor.{Val, Factor, LogD, Problem}
import vultura.factor.inference.{ParFunI, MargParI, JointMargI}
import vultura.util.{SSet, ArrayIndex}

import scala.runtime.ObjectRef

/**
 * Created by thomas on 05.09.14.
 */
case class LCBPGeneral(problem: Problem,
                       scheme: FactoredScheme,
                       inferer: Problem => MargParI with JointMargI,
                       maxUpdates: Long = 100000,
                       tol: Double = 1e-9) extends LcbpBase with ParFunI {
  //the scheme type we require
  override type ST = FactoredScheme

  val metaRing = LogD

  /** Maps the index of the conditioners in the original problem to a new index in the meta problem. */
  val mcVariablesIdx = new ArrayIndex[Int](scheme.allConditioners)

  val mcDomains: Array[Int] = mcVariablesIdx.elements.map(problem.domains)(collection.breakOut)

  sealed trait EnergyContrib {
    /** The index of the influencing conditioners within the original problem. */
    def conditioners: Array[Int]
    val mpVariables: Array[Int] = conditioners.map(mcVariablesIdx.forward)
    def isConstant = conditioners.isEmpty
    def sourceEdge(c: C): DoubleEdge
  }
  case class VariableContribution(vi: Int) extends EnergyContrib {
    val conditioners: Array[Int] = scheme.conditionVariablesOf(vi).toArray

    override def sourceEdge(c: C): DoubleEdge = {
      require(c.keySet == conditioners.toSet)
      FVariable(vi,c)
    }
  }
  case class FactorContribution(fi: Int) extends EnergyContrib {
    val conditioners: Array[Int] = scheme.conditionVariables(problem.factors(fi).variables.toSet).toArray

    override def sourceEdge(c: C): DoubleEdge = {
      require(c.keySet == conditioners.toSet)
      FFactor(fi,c)
    }
  }

  //we already drop constant factors here
  val contributions: IndexedSeq[EnergyContrib] =
    (problem.variableRange.map(VariableContribution) ++ (0 until problem.factors.size).map(FactorContribution))
      .filterNot(_.isConstant)

  private val ssetOfMPCliques: SSet[Int] = new SSet(contributions.map(_.mpVariables.toSet).toSet)
  /** The cliques of variables of the meta problem.
    * These will become the factors of the meta problem.*/
  val maxSet: IndexedSeq[Set[Int]] = ssetOfMPCliques.maximalSets.toIndexedSeq

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
      factors = for {
        scope <- maxSet
        contribFactors = contributions.filter(_.conditioners.forall(scope.contains)).map(buildFactor)
      } yield Factor.multiply(metaRing)(mcDomains)(contribFactors),
      domains = mcDomains,
      ring = metaRing)
  }

  case object MetaProblem extends LcbpMessage {
    override type InEdge = DoubleEdge
    override type TOut = ObjectRef[MargParI with JointMargI]

    override def copy(t: TOut): TOut = new ObjectRef(t.elem)

    /** Only has to create the value container (e.g. an array); the value will be taken from elsewhere. */
    override def create: TOut = new ObjectRef(inferer(Problem(IndexedSeq(),Array(),metaRing)))

    //input are all variable and factor contributions to the free energy
    override def inputs: IndexedSeq[DoubleEdge] = contributionValues.map{case (contr,c) => contr.sourceEdge(c)}

    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit = {
      (energyValues,problemRef) => problemRef.elem = inferer(buildProblem(energyValues.map(_.value)))
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
    val clique: Array[Int] = ssetOfMPCliques.maximalSuperSetsOf(fc.keySet.map(mcVariablesIdx.forward)).head.toArray
    private val vcVariables: Array[Int] = vc.keySet.toArray
    val vcCondition: Array[Val] = vcVariables.map(vc)
    //an ordering of the conditioners in vc as variables of the meta problem
    val vcVariablesMP: Array[Int] = vcVariables.map(mcVariablesIdx.forward)
    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit = { (ins,resultRef) =>
      val infResult: MargParI with JointMargI = ins(0).elem
      val belief = infResult.cliqueBelief(clique).normalize(metaRing)
      val summed = Factor.multiplyRetain(metaRing)(mcDomains)(Seq(belief),vcVariablesMP)
      resultRef.value = summed.eval(vcCondition, mcDomains)
    }
  }

  /** This must return a double-valued edge that computes the probability of condition `fc` given condition `vc`. */
  override def ccp(fc: C, vc: C): DoubleEdge = CCP(fc,vc)

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = ???
}
