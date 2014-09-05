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
           val tol: Double = 1e-9,
           val maxIterations: Int = 1000) extends MargParI {
  require(problem.ring == NormalD, "linear combination of messages only implemented for normal domain")

  //TODO make this work for the Log ring
  def linearCombination(weights: Array[Double], factors: IndexedSeq[Factor]): Factor = {
    assert(weights.size == factors.size)
    def elementWiseSum(a1: Array[Double], a2: Array[Double]) = a1.zip(a2).map{case (x1,x2) => x1 + x2}
    val weighted = factors.zip(weights).map{case (f,w) => f.map(_ * w)}
    weighted.reduce[Factor]{case (f1,f2) => f1.copy(values=elementWiseSum(f1.values,f2.values))}
  }

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
    * @param f Destination factor.
    * @param vc Variable condition. */
  case class V2F(v: Int, f: Factor, vc: Condition) extends FactorEdge {
    override type InEdge = F2VSummed
    def variables = Array(v)

    val conditionedBelief = Factor.deterministicMaxEntropy(Array(v),vc,problem.domains,problem.ring)

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[Factor]): TOut = fMul(ins :+ conditionedBelief).normalize(problem.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def inputs: IndexedSeq[InEdge] = for(of <- problem.factorsOfVariable(v) if of != f) yield F2VSummed(of,v,vc)
    override def toString: String = s"V2F:$v -> ${f.toBriefString} ${briefCondition(vc)}"
  }

  case class FactorBelief(f: Factor, fc: Condition) extends FactorEdge {
    override type InEdge = V2F
    override def variables: Array[Int] = f.variables
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[Factor]): TOut = fMul(ins :+ f).normalize(problem.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def inputs: IndexedSeq[V2F] = f.variables.map(v => V2F(v,f,scheme.superCondition(v,fc)))
    override def toString: String = s"FBel:${f.toBriefString} ${briefCondition(fc)}"
  }

  /** Message emerging from a factor node. In general this has to be combined with messages from differently conditioned
    * instances of the factor node to produce a message that can be multiplied into a variable belief.
    *
    * @param f Source factor.
    * @param v Destination variable.
    * @param fc Factor condition. */
  case class F2V(f: Factor, v: Int, fc: Condition) extends FactorEdge {
    override type InEdge = V2F
    def variables = Array(v)

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[Factor]): TOut =
      Factor.multiplyRetain(problem.ring)(problem.domains)(ins :+ f, Array(v)).normalize(problem.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def inputs: IndexedSeq[InEdge] = for(ov <- f.variables if ov != v) yield V2F(ov,f,scheme.superCondition(ov,fc))
    override def toString: String = s"F2V:${f.toBriefString} -> $v ${briefCondition(fc)}"
  }

  /** Message from a factor to a variable after summing over the refined conditions of the factor.
    * This message depends on the pre-summed F2V messages and on the distribution over conditions.
    * @param f Source factor.
    * @param v Destination variable.
    * @param vc Variable Condition.
    */
  case class F2VSummed(f: Factor, v: Int, vc: Condition) extends FactorEdge {
    type InEdge = LCBPEdge
    def variables = Array(v)

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute(ins: IndexedSeq[LCBPEdge#TOut]): TOut = {
      val dist = ins.head.asInstanceOf[Array[Double]]
      val rest  = ins.view.tail.map(_.asInstanceOf[Factor])
      assert(math.abs(dist.sum - 1) < 0.0001, "got something not normalized here")
      linearCombination(dist,rest.force.toIndexedSeq)
    }

    /** First entry is the condition distribution. The rest are the messages from the factor. */
    override def inputs: IndexedSeq[InEdge] = ConditionDistribution(subConditions,vc) +: subConditions.map(F2V(f,v,_))

    /** The conditions this sum combines over. */
    def subConditions: IndexedSeq[Condition] = scheme.subConditions(vc, f.variables).toIndexedSeq

    override def toString: String = s"F2VS:${f.toBriefString} -> $v ${briefCondition(vc)}"
  }

  case class VariableBelief(v: Int, vc: Condition) extends FactorEdge {
    override type InEdge = F2VSummed
    override def variables: Array[Int] = Array(v)

    val conditionedBelief: Factor = Factor.deterministicMaxEntropy(Array(v),vc,problem.domains,problem.ring)

    /** Create a (mutable?) representation of the initial value of this node. */
    def create: TOut = conditionedBelief

    /** The nodes this edge depends on. This must remain lazy. */
    override def inputs: IndexedSeq[F2VSummed] = problem.factorsOfVariable(v).map(f => F2VSummed(f,v,vc))

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
      variables.map{case (v,c,_) => VariableBelief(v,c)} ++ factors.map{case(f,fc) => FactorBelief(f,fc)}

    //third in tuple is number of neighbours, needed to compute the Bethe entropy approximation
    val variables: IndexedSeq[(Int,Condition,Int)] =
      problem.variables.map(v => (v, scheme.superCondition(v,condition), problem.factorsOfVariable(v).size))(collection.breakOut)
    val factors: IndexedSeq[(Factor,Condition)] =
      problem.factors.map(f => f -> scheme.superConditionJoint(f.variables,condition))(collection.breakOut)

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
    override def compute(ins: IndexedSeq[Double]): Array[Double] = LogD.decode(LogD.normalize((0 until conditions.size).map{ idx =>
      LogD.sumA(lookBack(idx).map(ins)(collection.breakOut))
    }(collection.breakOut)))

    override def toString: String = s"CDist: ${conditions.mkString} ${briefCondition(given)}"

    /** @return human readable representation of a value. */
    def printValue(v: TOut): String = s"(${v.mkString(",")})"
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
    def isConverged(e: LCBPEdge)(old: e.type#TOut, updated: e.type#TOut): Boolean = ((old,updated) match {
      case (o: Factor, u: Factor) => vultura.util.maxDiff(o.values,u.values)
      case (o: Array[Double], u: Array[Double]) => vultura.util.maxDiff(o,u)
      case (o: Double, u: Double) => math.abs(o - u)
    }) <= tol
  }

  object initializer extends EdgeValues[LCBPEdge]{
    override def hasEdge(e: LCBPEdge): Boolean = true
    override def edgeValue(e: LCBPEdge): e.type#TOut = e match {
      case FactorEdge(vars) => Factor.maxEntropy(vars,problem.domains,problem.ring).asInstanceOf[e.TOut]
      case ve: ValueEdge => problem.ring.one.asInstanceOf[e.TOut]
      case cd: ConditionDistribution => Array.fill(cd.conditions.size)(problem.ring.one).asInstanceOf[e.TOut]
    }
  }

  val calibrator: Calibrated[LCBPEdge] = new FIFOCalibrator[LCBPEdge](edges)(convTest,maxIterations,initializer)

  def iteration: Long = calibrator.iteration

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): Factor = ???

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(logZ)

  /** @return Natural logarithm of partition function. */
  override def logZ: Double = calibrator.edgeValue(LogPartition)
}