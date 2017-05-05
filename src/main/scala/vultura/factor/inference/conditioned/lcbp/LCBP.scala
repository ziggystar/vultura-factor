package vultura.factor.inference.conditioned.lcbp

import vultura.factor._
import vultura.factor.inference.MargParI
import vultura.factor.inference.calibration._
import vultura.factor.inference.conditioned._
import vultura.util._

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class LCBP(val problem: Problem,
           val scheme: GScheme,
           val tol: Double = 1e-12,
           val maxIterations: Int = 1000000) extends MargParI {

  override def ring: Ring[Double] = problem.ring

  type FactorIdx = Int

  sealed trait LCBPEdge extends Edge { self: Product =>
    override type InEdge <: LCBPEdge
  }

  /** Mixin for CEdges that have factor values. */
  trait FactorEdge extends LCBPEdge with HashMemo { self: Product =>
    type TOut = Factor
    def variables: Array[Int]
    val fMul: (IndexedSeq[Factor]) => Factor = Factor.multiply(problem.ring)(problem.domains)
  }

  object FactorEdge{
    def unapply(fe: FactorEdge): Some[Array[Int]] = Some(fe.variables)
  }

  trait ValueEdge extends LCBPEdge with HashMemo { self: Product =>
    type TOut = Double
  }

  def briefCondition(c: Condition): String = if(c.isEmpty) "" else "| " + c.map{case (k,v) => s"$k=$v"}.mkString(",")

  /** Messages from variable nodes to factor nodes. These can be directly used for the factor beliefs/computations,
    * because factors are always conditioned finer than variables. One such method will be used be differently conditioned
    * factors in general.
    *
    * @param v Source variable.
    * @param fi Destination factor.
    * @param vc Variable condition. */
  case class V2F(v: Int, fi: FactorIdx, vc: Condition) extends FactorEdge {
    override type InEdge = F2VSummed
    def variables = Array(v)

    val conditionedBelief = Factor.deterministicMaxEntropy(Array(v),vc,problem.domains,problem.ring)

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[Factor]): TOut = fMul(ins :+ conditionedBelief).normalize(problem.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def inputs: IndexedSeq[InEdge] = for(of <- problem.factorIdxOfVariable(v) if of != fi) yield F2VSummed(of,v,vc)
  }

  case class FactorBelief(fi: FactorIdx, fc: Condition) extends FactorEdge {
    override type InEdge = V2F
    def f = problem.factors(fi)
    override def variables: Array[Int] = f.variables
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[Factor]): TOut = fMul(ins :+ f).normalize(problem.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def inputs: IndexedSeq[V2F] = f.variables.map(v => V2F(v,fi,scheme.superCondition(v,fc)))
  }

  /** Message emerging from a factor node. In general this has to be combined with messages from differently conditioned
    * instances of the factor node to produce a message that can be multiplied into a variable belief.
    *
    * @param fi Source factor index.
    * @param v Destination variable.
    * @param fc Factor condition. */
  case class F2V(fi: FactorIdx, v: Int, fc: Condition) extends FactorEdge {
    override type InEdge = V2F
    def variables = Array(v)
    def f = problem.factors(fi)

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[Factor]): TOut =
      Factor.multiplyRetain(problem.ring)(problem.domains)(ins :+ f, Array(v)).normalize(problem.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def inputs: IndexedSeq[InEdge] = for(ov <- f.variables if ov != v) yield V2F(ov,fi,scheme.superCondition(ov,fc))
  }

  /** Message from a factor to a variable after summing over the refined conditions of the factor.
    * This message depends on the pre-summed F2V messages and on the distribution over conditions.
    * @param f Source factor.
    * @param v Destination variable.
    * @param vc Variable Condition.
    */
  case class F2VSummed(fi: FactorIdx, v: Int, vc: Condition) extends FactorEdge {
    type InEdge = LCBPEdge
    def variables = Array(v)

    def f = problem.factors(fi)

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[LCBPEdge#TOut]): TOut = {
      val dist = ins.head.asInstanceOf[Array[Double]]
      val rest  = ins.view.tail.map(_.asInstanceOf[Factor])
      Factor.linearCombination(dist, rest.force.toIndexedSeq, problem.ring)
    }

    /** First entry is the condition distribution. The rest are the messages from the factor. */
    override def inputs: IndexedSeq[InEdge] = ConditionDistribution(subConditions,vc) +: subConditions.map(F2V(fi,v,_))

    /** The conditions this sum combines over. */
    def subConditions: IndexedSeq[Condition] = scheme.subConditions(vc, f.variables).toIndexedSeq
  }

  case class VariableBelief(v: Int, vc: Condition) extends FactorEdge {
    override type InEdge = F2VSummed
    override def variables: Array[Int] = Array(v)

    val conditionedBelief: Factor = Factor.deterministicMaxEntropy(Array(v),vc,problem.domains,problem.ring)

    /** Create a (mutable?) representation of the initial value of this node. */
    def create: TOut = conditionedBelief

    /** The nodes this edge depends on. This must remain lazy. */
    override def inputs: IndexedSeq[F2VSummed] = problem.factorIdxOfVariable(v).map(fi => F2VSummed(fi,v,vc))

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[Factor]): Factor = fMul(ins :+ conditionedBelief).normalize(problem.ring)

    override def toString: String = s"VBel:$v ${briefCondition(vc)}"
  }

  val logFactors: IndexedSeq[Array[Double]] = problem.factors.map(factor => problem.ring.decode(factor.values).map(math.log))

  /** The weight of one elementary condition. */
  case class LogConditionWeight(condition: Condition) extends ValueEdge {
    override type InEdge = FactorEdge

    /** The nodes this edge depends on. This must remain lazy. */
    override def inputs: IndexedSeq[FactorEdge] =
      variables.map{case (v,c,_) => VariableBelief(v,c)} ++ factorConditions.zipWithIndex.map{case(fc,fi) => FactorBelief(fi,fc)}

    //third in tuple is number of neighbours, needed to compute the Bethe entropy approximation
    val variables: IndexedSeq[(Int,Condition,Int)] =
      problem.variables.map(v => (v, scheme.superCondition(v,condition), problem.factorIdxOfVariable(v).size))(collection.breakOut)
    val factorConditions: IndexedSeq[Condition] =
      problem.factors.map(f => scheme.superConditionJoint(f.variables,condition))(collection.breakOut)

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[Factor]): Double = {
      val view = ins.view
      val vBels = view.take(variables.length)
      val fBels = view.drop(variables.length)
      val logExpectsAndFactorEntropies = fBels.zip(logFactors).foldLeft(0d){
        case (acc,(fbel, factor)) => acc + problem.ring.expectation(fbel.values,factor) + problem.ring.entropy(fbel.values)
      }
      val weightedVariableEntropies = vBels.zip(variables).foldLeft(0d){
        case (acc,(vbel,(_,_,neighbours))) => acc + problem.ring.entropy(vbel.values) * (1 - neighbours)
      }
      logExpectsAndFactorEntropies + weightedVariableEntropies
    }

    override def toString: String = s"LogWeight ${briefCondition(condition)}"
  }

  /** Conditional distribution over conditions. This is required to combine the factor-to-variable messages,
    * when a factor is conditioned deeper than the adjacent variable.
    *
    * These are distributions in normal encoding.
    *
    * @param conditions The conditions of the factor.
    * @param given The condition of the variable.
    */
  case class ConditionDistribution(conditions: IndexedSeq[Condition], given: Condition) extends LCBPEdge {
    assert(
      conditions.forall(cond => cond.keySet.intersect(given.keySet).forall(k => cond(k) == given(k))),
      "contradictory conditioned distribution created")

    type InEdge = LogConditionWeight
    /** The output is in normal encoding! */
    type TOut = Array[Double]

    /** The conditional distribution over conditions is fed by the following factor/variable beliefs. */
    override lazy val inputs: IndexedSeq[InEdge] = inputConditions.map(LogConditionWeight)
    /** maps from index in `conditions` to contributing atomic conditions. */
    val lookup: Map[Int,Iterable[Condition]] = (for{
      (cond, idx) <- conditions.zipWithIndex
      refinedCond = cond ++ given //maps are required to be consistent
    subCond <- scheme.subConditions(refinedCond,problem.variables)
  } yield idx -> subCond).groupByMap(_._1,_._2)

    /** atomic conditions appear in in `input` in this order. */
    val inputConditions: IndexedSeq[Condition] = (lookup.flatMap(_._2)(collection.breakOut):Set[Condition]).toIndexedSeq

    /** maps from index in `conditions` to the indices of the contributing atomic conditions in `input`. */
    val lookBack: Map[Int,Iterable[Int]] = {
      // maps atomic conditions to their index in `inputs`
      val inputMap: Map[Condition, Int] = inputConditions.zipWithIndex.toMap
      lookup.map{case (k,v) => k -> v.map(inputMap)}
    }

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[Double]): Array[Double] = LogD.decode(LogD.normalize(conditions.indices.map{ idx =>
      LogD.sumA(lookBack(idx).map(ins)(collection.breakOut))
    }(collection.breakOut)))

    override def toString: String = s"CDist: ${conditions.mkString} ${briefCondition(given)}"

    /** @return human readable representation of a value. */
    override def prettyPrint(v: TOut): String = s"(${v.mkString(",")})"
  }
  
  /** Sums all LogConditionWeights. */
  case object LogPartition extends ValueEdge {
    override type InEdge = LogConditionWeight

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[Double]): Double = LogD.sumA(ins.toArray)

    /** The nodes this edge depends on. This must remain lazy. */
    override def inputs: IndexedSeq[LogConditionWeight] =
      scheme.jointConditions(problem.variables).map(LogConditionWeight)(collection.breakOut)

    override def toString: String = "LogPartition"
  }

  def edges: Set[LCBPEdge] = Edge.expand[LCBPEdge](LogPartition)


  object convTest extends ConvergenceTest[LCBPEdge] {
    def isConverged(e: LCBPEdge)(old: e.type#TOut, updated: e.type#TOut): Boolean = (e match {
      case ef: FactorEdge => vultura.util.maxDiff(old.asInstanceOf[ef.type#TOut].values,updated.asInstanceOf[ef.type#TOut].values)
      case cd: ConditionDistribution => vultura.util.maxDiff(old.asInstanceOf[cd.type#TOut],updated.asInstanceOf[cd.type#TOut])
      case vf: ValueEdge => math.abs(old.asInstanceOf[vf.type#TOut] - updated.asInstanceOf[vf.type#TOut])
    }) <= tol
  }

  object initializer extends EdgeValues[LCBPEdge]{
    override def hasEdge(e: LCBPEdge): Boolean = true
    override def edgeValue(e: LCBPEdge): e.type#TOut = e match {
      case FactorEdge(vars) => Factor.maxEntropy(vars,problem.domains,problem.ring).asInstanceOf[e.TOut]
      case ve: ValueEdge => problem.ring.one.asInstanceOf[e.TOut]
      case cd: ConditionDistribution => NormalD.normalize(Array.fill(cd.conditions.size)(NormalD.one)).asInstanceOf[e.TOut]
    }
  }

  val calibrator: FIFOCalibrator[LCBPEdge] = new FIFOCalibrator[LCBPEdge](edges)(convTest,maxIterations,initializer)

  def iteration: Long = calibrator.iteration

  implicit class RichCond(c: Condition){
    def implies(other: Condition): Boolean = other.forall{case (k,v) => c.get(k) contains v}
  }

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def encodedVarBelief(vi: Int): Factor = {
    val conditions = scheme.variableConditions(vi).toArray
    val conditionedBeliefs: IndexedSeq[Factor] = conditions.map(c => calibrator.edgeValue(VariableBelief(vi,c)))
    val logWeights: Array[Double] = conditions.map{c =>
      val matching = scheme.jointConditions(problem.variables).filter(_.implies(c))
      matching.foldLeft(LogD.zero){case (s,cm) => LogD.sum(s,calibrator.edgeValue(LogConditionWeight(cm)))}
    }
    val weights = NormalD.normalize(LogD.decode(logWeights))
    Factor.linearCombination(weights, conditionedBeliefs, problem.ring)
  }

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(logZ)

  /** @return Natural logarithm of partition function. */
  override def logZ: Double = calibrator.edgeValue(LogPartition)
}

object LCBP{
  def apply(scheme: FactoredScheme, tol: Double = 1e-12, maxIterations: Int = 1000000): LCBP =
    new LCBP(scheme.problem, scheme.toGScheme, tol, maxIterations)
}