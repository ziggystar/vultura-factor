package vultura.fastfactors.algorithms.conditioned

import vultura.util._
import vultura.fastfactors._
import vultura.fastfactors.algorithms.calibration.{Calibrator, CEdge}
import vultura.fastfactors.algorithms.MargParI
import vultura.util.graph.DotGraph

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class LCBP(val problem: Problem,
           val scheme: GScheme,
           val tol: Double = 1e-9,
           val maxIterations: Int = 1000,
           val exactConditions: Boolean = true) extends MargParI {
  require(problem.ring == NormalD, "linear combination of messages only implemented for normal domain")

  //TODO make this work for the Log ring
  def linearCombination(weights: Array[Double], factors: IndexedSeq[FastFactor]): FastFactor = {
    assert(weights.size == factors.size)
    def elementWiseSum(a1: Array[Double], a2: Array[Double]) = a1.zip(a2).map{case (x1,x2) => x1 + x2}
    val weighted = factors.zip(weights).map{case (f,w) => f.map(_ * w)}
    weighted.reduce[FastFactor]{case (f1,f2) => f1.copy(values=elementWiseSum(f1.values,f2.values))}
  }

  /** Mixin for CEdges that have factor values. */
  trait FactorEdge extends HashMemo { self: CEdge with Product =>
    type TOut = FastFactor

    def variables: Array[Int]
    /** Create a (mutable?) representation of the initial value of this node. */
    override def create: TOut = FastFactor.maxEntropy(variables,problem.domains,problem.ring)
    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double = vultura.util.maxDiff(r1.values,r2.values)

    val fMul: (IndexedSeq[FastFactor]) => FastFactor = FastFactor.multiply(problem.ring)(problem.domains)
  }

  trait ValueEdge extends HashMemo { self: CEdge with Product =>
    type TOut = java.lang.Double
    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double = math.abs(r1 - r2)
  }

  def briefCondition(c: Condition): String = if(c.isEmpty) "" else "| " + c.map{case (k,v) => s"$k=$v"}.mkString(",")

  /** Messages from variable nodes to factor nodes. These can be directly used for the factor beliefs/computations,
    * because factors are always conditioned finer than variables. One such method will be used be differently conditioned
    * factors in general.
    *
    * @param v Source variable.
    * @param f Destination factor.
    * @param vc Variable condition. */
  case class V2F(v: Int, f: FastFactor, vc: Condition) extends CEdge with FactorEdge {
    override type ETIn = F2VSummed
    def variables = Array(v)

    val conditionedBelief = FastFactor.deterministicMaxEntropy(Array(v),vc,problem.domains,problem.ring)

    /** Create a (mutable?) representation of the initial value of this node. */
    override def create: TOut = conditionedBelief

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => fMul(ins :+ conditionedBelief).normalize(problem.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = for(of <- problem.factorsOfVariable(v) if of != f) yield F2VSummed(of,v,vc)

    override def toString: String = s"V2F:$v -> ${f.toBriefString} ${briefCondition(vc)}"
  }

  case class FactorBelief(f: FastFactor, fc: Condition) extends CEdge with FactorEdge {
    override type ETIn = V2F
    override def variables: Array[Int] = f.variables
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => fMul(ins :+ f).normalize(problem.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = f.variables.map(v => V2F(v,f,scheme.superCondition(v,fc)))

    override def toString: String = s"FBel:${f.toBriefString} ${briefCondition(fc)}"
  }

  /** Message emerging from a factor node. In general this has to be combined with messages from differently conditioned
    * instances of the factor node to produce a message that can be multiplied into a variable belief.
    *
    * @param f Source factor.
    * @param v Destination variable.
    * @param fc Factor condition. */
  case class F2V(f: FastFactor, v: Int, fc: Condition) extends CEdge with FactorEdge {
    override type ETIn = V2F
    def variables = Array(v)

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins =>
      FastFactor.multiplyRetain(problem.ring)(problem.domains)(ins :+ f, Array(v)).normalize(problem.ring)

    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = for(ov <- f.variables if ov != v) yield V2F(ov,f,scheme.superCondition(ov,fc))

    override def toString: String = s"F2V:${f.toBriefString} -> $v ${briefCondition(fc)}"
  }

  /** Message from a factor to a variable after summing over the refined conditions of the factor.
    * This message depends on the pre-summed F2V messages and on the distribution over conditions.
    * @param f Source factor.
    * @param v Destination variable.
    * @param vc Variable Condition.
    */
  case class F2VSummed(f: FastFactor, v: Int, vc: Condition) extends CEdge with FactorEdge {
    type ETIn = CEdge
    def variables = Array(v)

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = in => {
      val dist = in.head.asInstanceOf[Array[Double]]
      val rest  = in.view.tail.map(_.asInstanceOf[FastFactor])
      assert(math.abs(dist.sum - 1) < 0.0001, "got something not normalized here")
      linearCombination(dist,rest.force.toIndexedSeq)
    }

    /** First entry is the condition distribution. The rest are the messages from the factor. */
    override def input: IndexedSeq[ETIn] = ConditionDistribution(subConditions,vc) +: subConditions.map(F2V(f,v,_))

    /** The conditions this sum combines over. */
    def subConditions: IndexedSeq[Condition] = scheme.subConditions(vc, f.variables).toIndexedSeq

    override def toString: String = s"F2VS:${f.toBriefString} -> $v ${briefCondition(vc)}"
  }

  case class VariableBelief(v: Int, vc: Condition) extends CEdge with FactorEdge {
    override type ETIn = F2VSummed
    override def variables: Array[Int] = Array(v)

    val conditionedBelief = FastFactor.deterministicMaxEntropy(Array(v),vc,problem.domains,problem.ring)

    /** Create a (mutable?) representation of the initial value of this node. */
    override def create: TOut = conditionedBelief

    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = problem.factorsOfVariable(v).map(f => F2VSummed(f,v,vc))

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => fMul(ins :+ conditionedBelief).normalize(problem.ring)

    override def toString: String = s"VBel:$v ${briefCondition(vc)}"
  }

  val logFactors: IndexedSeq[Array[Double]] = problem.factors.map(factor => problem.ring.decode(factor.values).map(math.log))

  require(problem.ring == NormalD, "condDist correction only implemented for normal messages")
  case class CondCorrection(v: Int, f: FastFactor, fc: Condition) extends CEdge with ValueEdge {
    
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => {
      val v2f = ins(0)
      val f2v = ins(1)
      val f2vSummed = ins(2)
      
      val correction = 
        for(i <- 0 until problem.domains(v))
        yield math.pow(v2f.values(i),v2f.values(i) * (f2v.values(i) - f2vSummed.values(i)))
      correction.product
    }

    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = IndexedSeq(
      V2F(v,f,scheme.superCondition(v,fc)),
      F2V(f,v,fc),
      F2VSummed(f,v,scheme.superCondition(v,fc)))

    /** Create a (mutable???) representation of the initial value of this node. */
    override def create: TOut = 0d

    override type ETIn = CEdge with FactorEdge
  }

  /** The weight of one elementary condition. */
  case class LogConditionWeight(condition: Condition) extends CEdge with ValueEdge {
    override type ETIn = CEdge with FactorEdge

    /** Create a (mutable?) representation of the initial value of this node. */
    override def create: TOut = problem.ring.one

    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] =
      variables.map{case (v,c,_) => VariableBelief(v,c)} ++ factors.map{case(f,fc) => FactorBelief(f,fc)}

    //third in tuple is number of neighbours, needed to compute the Bethe entropy approximation
    val variables: IndexedSeq[(Int,Condition,Int)] =
      problem.variables.map(v => (v, scheme.superCondition(v,condition), problem.factorsOfVariable(v).size))(collection.breakOut)
    val factors: IndexedSeq[(FastFactor,Condition)] =
      problem.factors.map(f => f -> scheme.superConditionJoint(f.variables,condition))(collection.breakOut)

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => {
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
  
  case class CorrectedLCW(condition: Condition) extends CEdge with ValueEdge {
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = (ins: IndexedSeq[java.lang.Double]) =>
      LogD.prodA((ins.tail.map(math.log(_)) :+ ins.head.doubleValue).toArray)

    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = IndexedSeq(LogConditionWeight(condition)) ++ corrections

    val corrections =
      for(f <- problem.factors; v <- f.variables)
      yield CondCorrection(v,f,scheme.superConditionJoint(f.variables,condition))

    /** Create a (mutable???) representation of the initial value of this node. */
    override def create: TOut = 1d

    override type ETIn = CEdge with ValueEdge
  }

  /** Conditional distribution over conditions. This is required to combine the factor-to-variable messages,
    * when a factor is conditioned deeper than the adjacent variable.
    *
    * These are distributions in normal encoding.
    *
    * @param conditions The conditions of the factor.
    * @param given The condition of the variable.
    */
  case class ConditionDistribution(conditions: IndexedSeq[Condition], given: Condition) extends CEdge {
    assert(
      conditions.forall(cond => cond.keySet.intersect(given.keySet).forall(k => cond(k) == given(k))),
      "contradictory conditioned distribution created")

    type ETIn = CEdge with ValueEdge
    /** The output is in normal encoding! */
    type TOut = Array[Double]

    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double = vultura.util.maxDiff(r1.toArray,r2.toArray)
    /** Create a (mutable?) representation of the initial value of this node. */
    override def create: TOut = Array.fill(conditions.size)(1d / conditions.size)

    /** The conditional distribution over conditions is fed by the following factor/variable beliefs. */
    override lazy val input: IndexedSeq[ETIn] =
      if(exactConditions) inputConditions.map(CorrectedLCW) else inputConditions.map(LogConditionWeight)
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
    override def compute: (IndexedSeq[TIn]) => TOut = ins => LogD.decode(LogD.normalize((0 until conditions.size).map{ idx =>
      LogD.sumA(lookBack(idx).map(i=> ins(i).doubleValue)(collection.breakOut))
    }(collection.breakOut)))

    override def toString: String = s"CDist: ${conditions.mkString} ${briefCondition(given)}"

    /** @return human readable representation of a value. */
    override def printValue(v: TOut): String = s"(${v.mkString(",")})"
  }
  
  /** Sums all LogConditionWeights. */
  case object LogPartition extends CEdge with ValueEdge {
    override type ETIn = CEdge with ValueEdge

    /** Create a (mutable?) representation of the initial value of this node. */
    override def create: TOut = 1d
    
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins =>
      LogD.sumA(ins.map(_.doubleValue)(collection.breakOut): Array[Double])

    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] =
      scheme.jointConditions(problem.variables).map(if(exactConditions) CorrectedLCW else LogConditionWeight)(collection.breakOut)

    override def toString: String = "LogPartition"
  }

  def edges: Set[CEdge] = CEdge.expand(Set(LogPartition))

  val calibrator = new Calibrator(edges,tol,maxIterations)

  def iteration: Int = calibrator.iteration

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): FastFactor = ???

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(logZ)

  /** @return Natural logarithm of partition function. */
  override def logZ: Double = calibrator.nodeState(LogPartition)

  def toDOT: DotGraph[CEdge] = calibrator.dot.nodeLabeled(n => n.toString + "\\n" + calibrator.nodeState(n).toString)
}