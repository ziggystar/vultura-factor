package vultura.fastfactors.algorithms.calibration

import vultura.fastfactors.{FastFactor, Problem}
import vultura.fastfactors.algorithms.InfAlg

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class BP_Cal(p: Problem, tol: Double = 1e-7, runInitially: Int = 1000) extends InfAlg {
  case class V2F(v: Int, f: FastFactor) extends CEdge {
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => FastFactor.multiply(p.ring)(p.domains)(ins).normalize(p.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = for(nf <- p.factorsOfVariable(v) if nf != f) yield F2V(nf,v)
    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double = vultura.util.maxDiff(r1.values,r2.values)
    /** Create a (mutable???) representation of the initial value of this node. */
    override def create: TOut = FastFactor.maxEntropy(Array(v),p.domains,p.ring)
    override type ETIn = F2V
    override type TOut = FastFactor
  }

  case class F2V(f: FastFactor, v: Int) extends CEdge {
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => FastFactor.multiplyRetain(p.ring)(p.domains)(ins :+ f, Array(v)).normalize(p.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = for(nv <- f.variables if nv != v) yield V2F(nv,f)
    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double = vultura.util.maxDiff(r1.values,r2.values)
    /** Create a (mutable???) representation of the initial value of this node. */
    override def create: TOut = FastFactor.maxEntropy(Array(v),p.domains,p.ring)
    override type ETIn = V2F
    override type TOut = FastFactor
  }

  val edges: Set[CEdge] = (for{
    f <- p.factors
    v <- f.variables
    edge <- Seq(F2V(f,v),V2F(v,f))
  } yield edge)(collection.breakOut)

  val calibrated = new Calibrator(edges,tol,runInitially)

  override def iteration: Int = 1

  val fMult: IndexedSeq[FastFactor] => FastFactor = FastFactor.multiply(p.ring)(p.domains)

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): FastFactor =
    fMult(p.factorsOfVariable(vi).map(F2V(_,vi)).map(n => calibrated.nodeState(n))).normalize(p.ring)
  def factorBelief(f: FastFactor): FastFactor = fMult(f.variables.map(v => calibrated.nodeState(V2F(v,f))) :+ f).normalize(p.ring)

  /** @return Partition function in encoding specified by `ring`. */
  override lazy val Z: Double = math.exp(logZ)
  override lazy val logZ: Double = {
    val ring = p.ring

      def expectation(p: Array[Double], f: Array[Double]): Double = {
        var result = 0d
        var i = 0
        while(i < p.length){
          if(p(i) != 0)
            result += p(i) * f(i)
          i += 1
        }
        require(!result.isNaN)
        result
      }
      def entropy(ps: Array[Double]) = {
        var result = 0d
        var i = 0
        while(i < ps.length){
          if(ps(i) != 0)
            result += ps(i) * math.log(ps(i))
          i += 1
        }
        require(!result.isNaN)
        -result
      }

      val factorExpectationAndEntropy = p.factors.map{f =>
        val values: Array[Double] = factorBelief(f).values
        expectation(ring.decode(values),ring.decode(f.values).map(math.log)) +
            entropy(ring.decode(values))
      }

      val variableEntropies = p.variables.map(v => (p.factorsOfVariable(v).size - 1) * entropy(ring.decode(variableBelief(v)).values))

      val clusterExpectationAndEntropySum: Double = factorExpectationAndEntropy.sum
      val variableEntropySum: Double = variableEntropies.sum
      val result = clusterExpectationAndEntropySum - variableEntropySum
      result
  }

  override def getProblem: Problem = p
}
