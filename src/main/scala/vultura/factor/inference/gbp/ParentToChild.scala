package vultura.factor.inference.gbp

import vultura.factor._
import vultura.factor.inference.MarginalI
import vultura.propagation._

import scala.reflect.ClassTag

case class ParentToChild(rg: RG, ring: Ring[Double]) {
  type VI = rg.VI
  type FI = rg.FI

  private val ps: ProblemStructure = rg.problemStructure
  val domains = ps.domains

  //  slightly generic code
  trait FactorNode extends NodeAD {
    type TRep = Factor
    def variables: Array[VI]

    def construct: Array[Double] = new Array[Double](variables.map(domains).product)
    def store(r: TRep, i: TImpl): Unit = System.arraycopy(r.values,0,i,0,i.length)
    def load(i: TImpl): TRep = Factor(variables,i.clone())
  }

  trait ProductRule[T <: FactorNode,F <: FactorNode] extends TypedRule[ADImpl,T,F] {
    override def typedImplementation(t: T): ADImpl#RuleType = productOf(typedDependenciesOf(t),t)
    def productOf(factors: IndexedSeq[FactorNode], result: FactorNode): ADImpl#RuleType = {
      val spt = SumProductTask(result.variables,domains,factors.map(_.variables)(collection.breakOut),ring)
      (ins,r) => {
        spt.sumProduct(ins,r)
        ring.normalizeInplace(r)
      }
    }
  }
  //end generic code

  case class Parameter(fi: FI) extends FactorNode {
    override def variables: Array[VI] = ps.scopeOfFactor(fi)
  }
  case class DownEdge(parent: Reg, child: Reg) extends FactorNode{
    override def variables: Array[VI] = child.variables.toArray.sorted
  }
  //region belief
  case class RBel(r: Reg) extends FactorNode{
    override def variables: Array[VI] = r.variables.toArray.sorted
  }

  val allEdges: IndexedSeq[DownEdge] = (for {
    p <- rg.regions
    c <- rg.children(p)
  } yield DownEdge(p,c))(collection.breakOut)

  val inboundForEdge: Map[DownEdge,IndexedSeq[DownEdge]] = allEdges.map{ case e@DownEdge(p,c) =>
    val pDescendants: Set[Reg] = selfAndDescendants(p)
    val edges: Set[DownEdge] = for{
      innerRegion <- pDescendants -- selfAndDescendants(c)
      outerRegion <- rg.parents(innerRegion) if !pDescendants(outerRegion)
    } yield DownEdge(outerRegion,innerRegion)
    e -> edges.toIndexedSeq
  }(collection.breakOut)

  val inboundForRegion: Map[Reg, IndexedSeq[DownEdge]] = rg.regions.map { r =>
    val descs: Set[Reg] = selfAndDescendants(r)
    val edges: Set[DownEdge] = for {
        inner <- descs
        outer <- rg.parents(inner) if !descs(outer)
      } yield DownEdge(outer,inner)
    r -> edges.toIndexedSeq
  }(collection.breakOut)

  def factorsForEdge(p: Reg, c: Reg): IndexedSeq[Parameter] =
    (p.factors -- c.factors).map(Parameter)(collection.breakOut)

  def factorsForRegion(r: Reg): IndexedSeq[Parameter] =
    selfAndDescendants(r).flatMap(d => d.factors.map(Parameter))(collection.breakOut)

  def selfAndDescendants(r: Reg): Set[Reg] = rg.descendants(r) + r

  object UpdateRule extends ProductRule[DownEdge,FactorNode]{
    override def tTag: ClassTag[DownEdge] = implicitly[ClassTag[DownEdge]]
    override def typedDependenciesOf(t: DownEdge): IndexedSeq[FactorNode] =
      factorsForEdge(t.parent, t.child) ++ inboundForEdge(t)

    override def typedImplementation(t: DownEdge): ADImpl#RuleType = { (x,y) =>
      super.typedImplementation(t)(x,y)
    }
  }

  object RBelRule extends ProductRule[RBel,FactorNode]{
    override def tTag: ClassTag[RBel] = implicitly[ClassTag[RBel]]
    override def typedDependenciesOf(t: RBel): IndexedSeq[FactorNode] = factorsForRegion(t.r) ++ inboundForRegion(t.r)
  }

  def calibrationProblem: CP[ADImpl] = CP(rg.regions.map(RBel), UpdateRule andThen RBelRule)

  def parametersFromProblem(p: Problem): IValuation[FactorNode] = new IValuation[Parameter] {
    override def isDefinedAt(n: Parameter): Boolean = true
    override def istore(n: Parameter, r: Parameter#TImpl): Unit =
      System.arraycopy(p.factors(n.fi).values,0,r,0,r.length)
  }.widen

  def margParI(iValuation: IValuation[FactorNode], p: Problem): MarginalI = new MarginalI{
    require(rg.problemStructure.isCompatible(p))
    val regionForVariable: Map[Int,RBel] =
      p.variables.map(v => v -> RBel(rg.regions.find(_.variables.contains(v)).get))(collection.breakOut)

    /** @return marginal distribution of variable in encoding specified by `ring`. */
    override def variableBelief(vi: Val): Factor = {
      val r = regionForVariable(vi)
      val builder: Array[Double] = r.construct
      val regResult = iValuation.istore(r,builder)
      Factor.multiplyRetain(ring)(p.domains)(Seq(r.load(builder)),Array(vi))
    }

    override def problem: Problem = p
  }
}
