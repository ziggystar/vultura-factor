package vultura.factor.inference.gbp

import vultura.factor._
import vultura.factor.inference.{ConvergenceStats, JointMargI, MargParI}
import vultura.propagation._

import scala.reflect.ClassTag

case class ParentToChild(rg: RegionGraph, ring: Ring[Double]) {
  type VI = rg.VI
  type FI = rg.FI

  type Reg = rg.Region

  if(vultura.factor.inference.logger.underlying.isWarnEnabled && !rg.Diagnosis.isValidRegionGraph)
    vultura.factor.inference.logger.warn("region graph has issues")

  private val ps: ProblemStructure = rg.problemStructure
  val domains = ps.domains

  //  slightly generic code
  trait FactorNode extends NodeAD {
    type TRep = Factor
    def variables: Array[VI]

    def construct: Array[Double] = new Array[Double](variables.map(domains).product)
    def store(r: TRep, i: TImpl): Unit = System.arraycopy(r.values,0,i,0,i.length)
    def load(i: TImpl): TRep = Factor(variables,i.clone())
    def make(iv: IValuation[FactorNode]): Factor = {
      val builder: Array[Double] = construct
      iv.istore(this,builder)
      load(builder)
    }
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
    override def variables: Array[VI] = rg.variablesOf(child).toArray.sorted
  }
  //region belief
  case class RBel(r: Reg) extends FactorNode{
    override def variables: Array[VI] = rg.variablesOf(r).toArray.sorted
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
    (rg.factorsOf(p) -- rg.factorsOf(c)).map(Parameter)(collection.breakOut)

  def factorsForRegion(r: Reg): IndexedSeq[Parameter] =
    selfAndDescendants(r).flatMap(d => rg.factorsOf(d).map(Parameter))(collection.breakOut)

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

  def constructResult(iValuation: IValuation[FactorNode], p: Problem): MargParI with JointMargI = new MargParI with JointMargI {
    require(rg.problemStructure.isCompatible(p))

    def regionBelief(r: Reg): Factor = RBel(r).make(iValuation)
    def decodedRegionBelief(r: Reg): Factor = {
      val f = regionBelief(r)
      f.copy(values = ring.decode(f.values))
    }

    /** @return Natural logarithm of partition function. */
    override lazy val logZ: Double = averageEnergy + entropy

    def regionEnergy(r: Reg): Double = {
      val logFactors = rg.factorsOf(r).toSeq.map(p.logFactor)
      val belief = decodedRegionBelief(r)
      val summedFactor = Factor.multiplyRetain(LogD)(p.domains)(logFactors,belief.variables)
      NormalD.expectation(belief.values,summedFactor.values)
    }
    lazy val averageEnergy: Double = rg.regions.foldLeft(0d)(_ + regionEnergy(_))
    lazy val entropy: Double = rg.regions.foldLeft(0d){case (sum,r) =>
      sum + ring.entropy(regionBelief(r).values) * rg.weightOf(r)
    }

    val regionForVariable: Map[Int,RBel] =
      p.variables.map(v => v -> RBel(rg.regions.find(r => rg.variablesOf(r).contains(v)).get))(collection.breakOut)

    /** @return Normalized belief over given variables in encoding specified by problem ring. */
    override def cliqueBelief(vars: Array[Var]): Factor = {
      val set: Set[VI] = vars.toSet
      rg.regions.find(r => set.subsetOf(rg.variablesOf(r)))
        .map(cb => Factor.multiplyRetain(p.ring)(p.domains)(Seq(RBel(cb).make(iValuation)),vars))
        .getOrElse(throw new RuntimeException("joint marginals not available, as there is no containing region"))
    }

    /** @return marginal distribution of variable in encoding specified by `ring`. */
    override def encodedVarBelief(vi: Val): Factor =
      Factor.multiplyRetain(ring)(p.domains)(Seq(regionForVariable(vi).make(iValuation)),Array(vi))

    override def problem: Problem = p
  }
}

object ParentToChild{
  def infer(rg: RegionGraph, problem: Problem, tol: Double = 1e-12, maxIter: Long = 100000): (MargParI with JointMargI,ConvergenceStats) = {
    require(rg.problemStructure.isCompatible(problem), "problem doesn't fit region graph")
    val ptc = ParentToChild(rg, problem.ring)

    val neutralValuation: RValuation[ptc.FactorNode] = new RValuation[ptc.FactorNode]{
      override def isDefinedAt(n: ptc.FactorNode): Boolean = true
      override def rval(n: ptc.FactorNode): n.TRep = Factor.maxEntropy(n.variables,ptc.domains,ptc.ring)
    }

    val calibrator = new RoundRobinAD(ptc.calibrationProblem,MaxDiff,neutralValuation.widen.toIVal)

    val result = calibrator.calibrate(ptc.parametersFromProblem(problem).widen, maxDiff = tol, maxLoops = maxIter)
    (
      ptc.constructResult(result.ival.asInstanceOf[IValuation[ptc.FactorNode]],problem),
      ConvergenceStats(result.totalUpdates,result.maxDiff,result.isConverged))
  }
}
