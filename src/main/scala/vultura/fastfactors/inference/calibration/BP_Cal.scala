package vultura.fastfactors.inference.calibration

import vultura.fastfactors.{FastFactor, Problem}
import vultura.fastfactors.inference.MargParI

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class BP_Cal(val problem: Problem, val tol: Double = 1e-7, runInitially: Int = 1000) extends MargParI  {
  case class V2F(v: Int, f: FastFactor) extends CEdge {
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => FastFactor.multiply(problem.ring)(problem.domains)(ins).normalize(problem.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = for(nf <- problem.factorsOfVariable(v) if nf != f) yield F2V(nf,v)
    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double = vultura.util.maxDiff(r1.values,r2.values)
    /** Create a (mutable???) representation of the initial value of this node. */
    override def create: TOut = FastFactor.maxEntropy(Array(v),problem.domains,problem.ring)
    override type ETIn = F2V
    override type TOut = FastFactor
  }

  case class F2V(f: FastFactor, v: Int) extends CEdge {
    /** Compute the value of this node given the values of the independent nodes. */
    override def compute: (IndexedSeq[TIn]) => TOut = ins => FastFactor.multiplyRetain(problem.ring)(problem.domains)(ins :+ f, Array(v)).normalize(problem.ring)
    /** The nodes this edge depends on. This must remain lazy. */
    override def input: IndexedSeq[ETIn] = for(nv <- f.variables if nv != v) yield V2F(nv,f)
    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double = vultura.util.maxDiff(r1.values,r2.values)
    /** Create a (mutable???) representation of the initial value of this node. */
    override def create: TOut = FastFactor.maxEntropy(Array(v),problem.domains,problem.ring)
    override type ETIn = V2F
    override type TOut = FastFactor
  }

  val edges: Set[CEdge] = (for{
    f <- problem.factors
    v <- f.variables
    edge <- Seq(F2V(f,v),V2F(v,f))
  } yield edge)(collection.breakOut)

  val calibrated = new Calibrator(edges,tol,runInitially)

  def iteration: Int = 1

  val fMult: IndexedSeq[FastFactor] => FastFactor = FastFactor.multiply(problem.ring)(problem.domains)

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): FastFactor =
    fMult(problem.factorsOfVariable(vi).map(F2V(_,vi)).map(n => calibrated.nodeState(n))).normalize(problem.ring)
  def factorBelief(f: FastFactor): FastFactor = fMult(f.variables.map(v => calibrated.nodeState(V2F(v,f))) :+ f).normalize(problem.ring)

  /** @return Partition function in encoding specified by `ring`. */
  override lazy val Z: Double = math.exp(logZ)
  override lazy val logZ: Double = {
    val ring = problem.ring

    val factorExpectationAndEntropy = problem.factors.map{f =>
      val values: Array[Double] = factorBelief(f).values
      ring.expectation(values,ring.decode(f.values).map(math.log)) + ring.entropy(values)
    }

    val variableEntropies = problem.variables.map(v => (problem.factorsOfVariable(v).size - 1) * ring.entropy(variableBelief(v).values))

    val clusterExpectationAndEntropySum: Double = factorExpectationAndEntropy.sum
    val variableEntropySum: Double = variableEntropies.sum
    val result = clusterExpectationAndEntropySum - variableEntropySum
    result
  }

  def toDOT: String = calibrated.dot.nodeLabeled(n => n.toString + "\\n" + calibrated.nodeState(n).toString).dotString
}
