package vultura.factor.inference.conditioned.lcbp

import vultura.factor._
import vultura.factor.inference.conditioned.Condition

/**
 * Implementation of LCBP using BP for meta-inference.
 * Implementation only for factored schemes.
 */
class LcbpMetaBP(val scheme: FactoredScheme, val maxUpdates: Long = 1000000, val tol: Double = 1e-12) extends LcbpBase{
  override type ST = FactoredScheme

  //meta variable index
  type MVI = Int
  //meta factor index
  type MFI = Int

  val meta: ProblemStructure = ???
  def metaFactorEdge(fi: MFI): FactorEdge = ???
  def metaRing: Ring[Double] = LogD
  def conditionVarToMetaVar(v: Int): MVI = ???
  def conditionToMetaCondition(c: Condition): Condition = c.map{case (k,v) => conditionVarToMetaVar(k) -> v}
  def containingMetaClique(mvs: Iterable[MVI]): MFI = ???

//  case class MetaFactor(metaVariables: Array[MVI], )
  case class MetaV2F(v: MVI, fi: MFI) extends FactorEdge {
    override def variables: Array[MVI] = Array(v)
    override type InEdge = MetaF2V
    override def inputs: IndexedSeq[InEdge] = for(nf <- meta.factorIdxOfVariable(v) if nf != fi) yield MetaF2V(nf,v)
    def mCompute() = {
      //this must be lazy, otherwise inputs gets called indefinitely
      val spTask = SumProductTask(
        remainingVars = Array(v),
        domainSizes = meta.domains,
        inputs.map(f2v => Array(f2v.v))(collection.breakOut),
        metaRing
      )
      (ins: IndexedSeq[Array[Double]], result: Array[Double]) => {
        spTask.sumProduct(ins,result)
        metaRing.normalizeInplace(result)
      }
    }
  }

  case class MetaF2V(fi: MFI, v: MVI) extends FactorEdge {
    override def variables: Array[MVI] = Array(v)
    override type InEdge = FactorEdge
    //first input is the factor value, tail are the incoming messages (except the one from `v`)
    override def inputs: IndexedSeq[InEdge] = metaFactorEdge(fi) +: (for(nv <- meta.scopeOfFactor(fi) if nv != v) yield MetaV2F(nv,fi))
    override def mCompute(): (IndexedSeq[Array[Double]], Array[Double]) => Unit =
      constructSPTask(inputs.map(_.variables),variables,Seq(),meta.domains,metaRing)
  }

  case class MetaFBel(fi: MFI) extends FactorEdge {
    override def variables: Array[Int] = meta.scopeOfFactor(fi)
    override type InEdge = MetaV2F
    override def inputs: IndexedSeq[InEdge] = for(mvi <- meta.scopeOfFactor(fi)) yield MetaV2F(mvi,fi)
    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit =
      constructSPTask(inputs.map(_.variables), variables, Seq(), meta.domains, metaRing)
  }

  case class CCP(fc: C, vc: C) extends DoubleEdge {
    override type InEdge = MetaFBel
    val mfc = conditionToMetaCondition(fc)
    val mvc = conditionToMetaCondition(vc)

    //the factor belief of the meta problem we require
    val edge: MetaFBel = MetaFBel(containingMetaClique(mfc.keys))
    //we marginalize and retain these variables
    val retainVars: Array[Int] = mvc.keys.toArray.sorted
    //and that's the assignment we are interested in
    val conditionValues: Array[Int] = retainVars.map(mvc)

    override def inputs: IndexedSeq[InEdge] = IndexedSeq(edge)

    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], DoubleRef) => Unit = (fb,res) => {
      val rLog = Factor
        .multiplyRetain(metaRing)(meta.domains)(Seq(edge.makeFactor(fb.head)), retainVars) //marginalize to mvc condition
        .eval(conditionValues, meta.domains) //select proper value
      res.value = math.exp(rLog)
    }
  }

  /** This must return a double-valued edge that computes the partition function of the meta problem. */
  override def cdLogZ: DoubleEdge = ???

  /** This must return a double-valued edge that computes the probability of condition `fc` given condition `vc`. */
  override def ccp(fc: C, vc: C): DoubleEdge = CCP(fc,vc)
}
