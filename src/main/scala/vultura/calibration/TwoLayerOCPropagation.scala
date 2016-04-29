package vultura.calibration

import vultura.factor._
import vultura.factor.inference.{VarBeliefFromRegionBelief, VariationalResult, RegionBeliefs}
import vultura.inference.gbp.TwoLayerOC

/** Message-Passing algorithm implementing two-way message passing on bipartite region graphs.
  * This makes the algorithm applicable to junction graphs, bethe graphs, cluster graphs.
  * Application to bethe graphs yields Beleif Propagation on factor graphs.
  * @param rg
  * @param ring
  */
class TwoLayerOCPropagation(val rg: TwoLayerOC, val ring: Ring[Double])
  extends CalProblem with ResultBuilder[RegionBeliefs[TwoLayerOC#Region] with VariationalResult] { outer =>

  type Region = rg.Region

  override type N = Node with FactorNode

  override type Parameter = IndexedSeq[Factor]

  val ps = rg.problemStructure
  type Small = rg.Small
  type Large = rg.Large

  trait FactorNode {self: Node =>
    def variables: Array[Int]
    def arraySize = variables.map(rg.problemStructure.domains).product
  }

  case class ParamNode(large: Large) extends ParameterNode with FactorNode{
    val variables: Array[Int] = large.variables.toArray.sorted
  }

  case class S2L(small: Small, large: Large) extends ComputedNode with FactorNode {
    lazy val dependencies: IndexedSeq[L2S] = small.parents.filterNot(_ == large).map(ol => L2S(ol,small))(collection.breakOut)

    val variables: Array[Int] = rg.edgeVariables(large,small).toArray.sorted

    lazy val task: (IndexedSeq[IR], IR) => Unit =
      SumProductTask(
        variables,
        ps.domains,
        dependencies.map(_.variables)(collection.breakOut), ring).sumProductNormalize
    override def compute(ins: Array[IR], result: IR): Unit = task(ins,result)
  }

  case class L2S(large: Large, small: Small) extends ComputedNode with FactorNode {
    val variables: Array[Int] = rg.edgeVariables(large,small).toArray.sorted
    lazy val dependencies: IndexedSeq[N] = ParamNode(large) +: large.children.filterNot(_ == small).map(os => S2L(os,large))(collection.breakOut)
    lazy val task: (IndexedSeq[IR], IR) => Unit =
      SumProductTask(
        variables,
        ps.domains,
        dependencies.map(_.variables)(collection.breakOut),
        ring
      ).sumProductNormalize
    override def compute(ins: Array[IR], result: IR): Unit = task(ins,result)
  }

  /** The set of nodes defined by this problem. */
  override def nodes: Set[N] =  ((for {
    s <- rg.smallRegions
    l <- s.parents
    m <- Seq(L2S(l, s), S2L(s, l))
  } yield m) ++ rg.largeRegions.map(ParamNode))(collection.breakOut)

  /** Constructs a new initial value for each edge. */
  override def initializer(factors: IndexedSeq[Factor]): N => IR = {
    case pn@ParamNode(l) => Factor.multiplyRetain(ring)(ps.domains)(l.factors.toIndexedSeq.map(factors),pn.variables).values
    case otherwise => Factor.maxEntropy(otherwise.variables, ps.domains, ring).values
  }

  override def buildResult(valuation: (N) => IR): RegionBeliefs[TwoLayerOC#Region] with VariationalResult =
    new RegionBeliefs[TwoLayerOC#Region] with VariationalResult with VarBeliefFromRegionBelief[TwoLayerOC#Region] {
      override def ring: Ring[Double] = outer.ring
      override def problem: ProblemStructure = rg.problemStructure

      override def regions: Set[TwoLayerOC#Region] = rg.regions.map(identity) //set invariance...

      override def scopeOfRegion(region: TwoLayerOC#Region): Set[Int] = rg.variablesOf(region.asInstanceOf[Region])

      override def averageEnergy: Double = rg.largeRegions.foldLeft(0d){case (ae,large) =>
        val toLog = if(ring == LogD) identity[Array[Double]]_ else (_:Array[Double]).map(math.log)
        val logFactor = toLog(valuation(ParamNode(large)))
        val rBel = regionBelief(large)
        val regionEnergy = NormalD.expectation(rBel.values,logFactor)
        ae + rg.weightOf(large) * regionEnergy
      }

      override def entropy: Double = rg.regions.foldLeft(0d){case (h,r) =>
        val hr = NormalD.entropy(regionBelief(r).values)
        h + rg.weightOf(r) * hr
      }

      /** In normal encoding. */
      override def regionBelief(region: TwoLayerOC#Region): Factor = {
        val inMsgs = region match {
          case s: Small => s.parents.map(l => L2S(l,s))
          case l: Large => l.children.map(s => S2L(s,l)) :+ ParamNode(l)
          case _        => sys.error("supplying a region of the wrong region graph")
        }
        Factor.multiply(ring)(ps.domains)(inMsgs.map(msg => Factor(msg.variables,valuation(msg)))(collection.breakOut))
          .normalize(ring)
      }
    }
}
