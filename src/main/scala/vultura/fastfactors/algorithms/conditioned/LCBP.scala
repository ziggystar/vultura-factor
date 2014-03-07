package vultura.fastfactors.algorithms.conditioned

import vultura.util._
import vultura.fastfactors._
import vultura.fastfactors.algorithms.calibration.{Calibrator, CEdge}
import vultura.fastfactors.algorithms.InfAlg

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class LCBP(p: Problem,
           scheme: GScheme = GScheme(),
           tol: Double = 1e-9,
           maxIterations: Int = 1000) extends InfAlg {
  require(p.ring == NormalD, "linear combination of messages only implemented for normal domain")

  //TODO make this work for the Log ring
  def linearCombination(weights: Array[Double], factors: IndexedSeq[FastFactor]): FastFactor = {
    require(weights.size == factors.size)
    def elementWiseSum(a1: Array[Double], a2: Array[Double]) = a1.zip(a2).map{case (x1,x2) => x1 + x2}
    val weighted = factors.zip(weights).map{case (f,w) => f.map(_ * w)}
    weighted.reduce[FastFactor]{case (f1,f2) => f1.copy(values=elementWiseSum(f1.values,f2.values))}
  }

  /** Mixin for CEdges that have factor values. */
  trait FactorEdge { self: CEdge =>
    type TOut = FastFactor

    def variables: Array[Int]
    /** Create a (mutable???) representation of the initial value of this node. */
    override def create: TOut = FastFactor.maxEntropy(variables,p.domains,p.ring)
    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double = vultura.util.maxDiff(r1.values,r2.values)

    val fMul: (IndexedSeq[FastFactor]) => FastFactor = FastFactor.multiply(p.ring)(p.domains)
  }

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

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => fMul(ins).normalize(p.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = for(of <- p.factorsOfVariable(v) if of != f) yield F2VSummed(of,v,vc)
  }

  case class FactorBelief(f: FastFactor, fc: Condition) extends CEdge with FactorEdge {
    override type ETIn = V2F
    override def variables: Array[Int] = f.variables
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => fMul(ins :+ f).normalize(p.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = f.variables.map(v => V2F(v,f,scheme.superCondition(v,fc)))
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
      FastFactor.multiplyRetain(p.ring)(p.domains)(ins :+ f, Array(v)).normalize(p.ring)

    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = for(ov <- f.variables if ov != v) yield V2F(ov,f,scheme.superCondition(ov,fc))
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
  }

  case class VariableBelief(v: Int, vc: Condition) extends CEdge with FactorEdge {
    override type ETIn = F2VSummed
    override def variables: Array[Int] = Array(v)

    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = p.factorsOfVariable(v).map(f => F2VSummed(f,v,vc))

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => fMul(ins).normalize(p.ring)
  }

  /** The weight of one elementary condition. */
  case class LogConditionWeight(condition: Condition) extends CEdge {
    override type ETIn = CEdge with FactorEdge
    override type TOut = java.lang.Double

    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double = math.abs(r1 - r2)

    /** Create a (mutable???) representation of the initial value of this node. */
    override def create: TOut = p.ring.one

    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] =
      variables.map{case (v,c,_) => VariableBelief(v,c)} ++ factors.map{case(f,fc) => FactorBelief(f,fc)}

    //third in tuple is number of neighbours, needed to compute the Bethe entropy approximation
    val variables: IndexedSeq[(Int,Condition,Int)] =
      p.variables.toIndexedSeq.map(v => (v, scheme.superCondition(v,condition), p.neighboursOf(v).size))
    val factors: IndexedSeq[(FastFactor,Condition)] =
      p.factors.toIndexedSeq.map(f => f -> scheme.superConditionJoint(f.variables,condition))

  /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => {
      val vBels = ins.take(variables.length)
      val fBels = ins.drop(variables.length)
      //TODO optimize: cache the log factors
      val logExpects = fBels.zip(factors).map{case (fbel, (factor,_)) => p.ring.expectation(fbel.values,p.ring.decode(factor.values).map(math.log))}.sum
      val factorEntropies = fBels.map(fb => p.ring.entropy(fb.values)).sum
      val weightedVariableEntropies = vBels.zip(variables).map{case (vbel,(_,_,neighbours)) => p.ring.entropy(vbel.values) * (1 - neighbours)}.sum
      logExpects + factorEntropies + weightedVariableEntropies
    }
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
    require(
      conditions.forall(cond => cond.keySet.intersect(given.keySet).forall(k => cond(k) == given(k))),
      "contradictory conditioned distribution created")

    type ETIn = LogConditionWeight
    /** The output is in normal encoding! */
    type TOut = Array[Double]

    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double = vultura.util.maxDiff(r1.toArray,r2.toArray)
    /** Create a (mutable???) representation of the initial value of this node. */
    override def create: TOut = Array.fill(conditions.size)(1d / conditions.size)

    /** The conditional distribution over conditions is fed by the following factor/variable beliefs. */
    override def input: IndexedSeq[ETIn] = inputConditions.map(LogConditionWeight)
    /** maps from index in `conditions` to contributing atomic conditions. */
    val lookup: Map[Int,Iterable[Condition]] = (for{
      (cond, idx) <- conditions.zipWithIndex
      refinedCond = cond ++ given //maps are required to be consistent
      superCond = scheme.superConditionJoint(p.variables,refinedCond)
    } yield idx -> superCond).groupByMap(_._1,_._2)

    /** atomic conditions appear in in `input` in this order. */
    val inputConditions: IndexedSeq[Condition] = lookup.flatMap(_._2).toSet.toIndexedSeq
    /** maps atomic conditions to their index in `inputs`. */
    val inputMap: Map[Condition, Int] = inputConditions.zipWithIndex.toMap

    /** maps from index in `conditions` to the indices of the contributing atomic conditions in `input`. */
    val lookBack: Map[Int,Iterable[Int]] = lookup.map{case (k,v) => k -> v.map(inputMap)}

    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => LogD.decode(LogD.normalize((0 until conditions.size).map{ idx =>
      LogD.sumA(lookBack(idx).map(i=> ins(i).doubleValue)(collection.breakOut))
    }(collection.breakOut)))
  }
  
  /** Sums all LogConditionWeights. */
  case object LogPartition extends CEdge {
    override type ETIn = LogConditionWeight
    override type TOut = java.lang.Double
    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double = math.abs(r1 - r2)
    /** Create a (mutable???) representation of the initial value of this node. */
    override def create: TOut = 1d
    
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins =>
      LogD.sumA(ins.map(_.doubleValue)(collection.breakOut): Array[Double])

    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] =
      scheme.jointConditions(p.variables).map(LogConditionWeight)(collection.breakOut)
  }

  def edges: Set[CEdge] = CEdge.expand(Set(LogPartition))

  val calibrator = new Calibrator(edges,tol,maxIterations)

  override def iteration: Int = calibrator.iteration

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): FastFactor = ???

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(logZ)

  /** @return Natural logarithm of partition function. */
  override def logZ: Double = calibrator.nodeState(LogPartition)

  override def getProblem: Problem = p
}