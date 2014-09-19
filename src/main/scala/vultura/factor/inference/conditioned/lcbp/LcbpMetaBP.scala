package vultura.factor.inference.conditioned.lcbp

import vultura.factor.{Ring, SumProductTask, Factor}

import scala.collection.mutable

/**
 * Created by thomas on 17.09.14.
 */
class LcbpMetaBP(val scheme: FactoredScheme, val maxUpdates: Long = 1000000, val tol: Double = 1e-12) extends LcbpBase{
  override type ST = FactoredScheme

  //meta variable index
  type MVI = Int
  //meta factor index
  type MFI = Int
  def factorValue(fi: MFI): FactorEdge = ???
  def metaFactorStructure(fi: MFI): Array[MVI] = ???
  def metaFactorIdxOfVariable(v: MVI): Array[MFI] = ???
  def variablesOfMetaFactor(fi: MFI): Array[MVI] = ???
  def metaVariableDomains: Array[MVI] = ???
  def metaRing: Ring[Double] = ???

//  case class MetaFactor(metaVariables: Array[MVI], )
  case class MetaV2F(v: MVI, fi: MFI) extends FactorEdge {
    override def variables: Array[MVI] = Array(v)
    override type InEdge = MetaF2V
    override def inputs: IndexedSeq[InEdge] = for(nf <- metaFactorIdxOfVariable(v) if nf != fi) yield MetaF2V(nf,v)
    def mCompute() = {
      //this must be lazy, otherwise inputs gets called indefinitely
      val spTask = SumProductTask(
        remainingVars = Array(v),
        domainSizes = metaVariableDomains,
        inputs.map(f2v => Array(f2v.v))(collection.breakOut),
        metaRing
      )
      (ins: IndexedSeq[Array[Double]], result: Array[Double]) => {
        spTask.sumProduct(ins,result)
        problem.ring.normalizeInplace(result)
      }
    }
  }

  case class MetaF2V(fi: MFI, v: MVI) extends FactorEdge {

    override def variables: Array[MVI] = Array(v)

    override type InEdge = FactorEdge
    override def inputs: IndexedSeq[InEdge] = for(nv <- variablesOfMetaFactor(fi) if nv != v) yield MetaV2F(nv,fi)
    override def mCompute() = {
      val spTask = SumProductTask(
        remainingVars = Array(v),
        domainSizes = metaVariableDomains,
        (inputs.map(v2f => v2f.variables)(collection.breakOut) :+ variablesOfMetaFactor(fi))(collection.breakOut),
        metaRing)
      val factorHolder = new mutable.ArraySeq[Array[Double]](variablesOfMetaFactor(fi).size)

//      (ins: IndexedSeq[Array[Double]], result: Array[Double]) => {
//        var i = 0
//        while(i < ins.size){
//          factorHolder(i) = ins(i)
//          i += 1
//        }
//        factorHolder(i) = f.values
//
//        spTask.sumProduct(factorHolder,result)
//        problem.ring.normalizeInplace(result)
//      }
      ???
    }
  }

  /** This must return a double-valued edge that computes the partition function of the meta problem. */
  override def cdLogZ: DoubleEdge = ???

  /** This must return a double-valued edge that computes the probability of condition `fc` given condition `vc`. */
  override def ccp(fc: C, vc: C): DoubleEdge = ???
}
