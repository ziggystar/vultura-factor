package vultura.factor.inference.conditioned.lcbp

import vultura.factor._
import vultura.factor.inference.calibration.MEdge

/** This trait defines the message for LCBP, excluding calculation of the meta problem.
  * This means it still requires a mixin of type `dunno yet` to provide inference on the meta problem.
 */
trait LcbpBase {
  type ST <: Scheme
  type C = ST#GC
  def scheme: ST
  def problem: Problem = scheme.problem

  def linearCombination(_weights: Array[Double], factors: IndexedSeq[Array[Double]]): Array[Double] = {
    assert(_weights.size == factors.size)

    //we normalize here, since otherwise all subclasses would have to take care of this
    val weights = NormalD.normalize(_weights)

    def sumFactors(fs: Array[Array[Double]], ring: Ring[Double]): Array[Double] =
    fs.transpose.map(ring.sumA)(collection.breakOut)

    if(problem.ring == NormalD) {
      val weighted: Array[Array[Double]] = factors.zip(weights).map{case (f,w) => f.map(_ * w)}(collection.breakOut)
      sumFactors(weighted,NormalD)
    } else {
      val weighted: Array[Array[Double]] = factors.zip(weights).map{ case (f,w) =>
        val logWeight = math.log(w)
        f.map(_ + logWeight)
      }(collection.breakOut)
      sumFactors(weighted,LogD)
    }
  }

  trait LcbpMessage extends MEdge {self: Product =>
    def drawColor: String = "black"
    override def dotNodeOption: Seq[String] = super.dotNodeOption :+ s"color = $drawColor"
  }

  trait ArrayEdge extends LcbpMessage {self: Product =>
    final type TOut = Array[Double]
    override def prettyPrint(t: TOut): String = t.map(x => f"$x%.4f").mkString(",")
  }

  trait FactorEdge extends ArrayEdge {self: Product =>
    def variables: Array[Int]
    def domains: Array[Int]

    def create: TOut = new Array[Double](variables.map(problem.domains).product)
    def copy(t: TOut): TOut = t.clone()
    def makeFactor(t: TOut): Factor = Factor(variables.clone(),t.clone())
  }

  object FactorEdge{
    def unapply(fe: FactorEdge): Option[Array[Int]] = Some(fe.variables)
  }

  trait BaseFactorEdge extends FactorEdge {self: Product =>
    def domains = problem.domains
  }

  trait BPEdge extends BaseFactorEdge {self: Product =>
    def v: Int
    def f: Int

    final override def variables: Array[Int] = Array(v)
  }

  class DoubleRef(v: Double = 0d) extends Cloneable {
    var value: Double = v
    override def toString: String = value.toString
    override def clone(): AnyRef = new DoubleRef(value)
  }

  object DoubleRef{
    def unapply(x: DoubleRef): Option[Double] = Some(x.value)
  }

  trait DoubleEdge extends LcbpMessage {self: Product =>
    final type TOut = DoubleRef
    def create: TOut = new DoubleRef()
    def copy(t: TOut): TOut = new DoubleRef(t.value)

    override def prettyPrint(t: TOut): String = f"${t.value}%.4f"
  }

  /** Construct a [[SumProductTask]].
    *
    * @param inputFactors Just the variables of the factors the task is going to expect.
    * @param retainedVariables The variables not to sum over.
    * @param fixedFactors Add these factors to the result.
    */
  def constructSPTask(inputFactors: IndexedSeq[Array[Int]],
                      retainedVariables: Array[Int],
                      fixedFactors: Seq[Factor] = Seq(),
                      domains: Array[Int] = problem.domains,
                      ring: Ring[Double] = problem.ring): (IndexedSeq[Array[Double]], Array[Double]) => Unit = {
    //this must be lazy, otherwise inputs gets called indefinitely
    val spTask = SumProductTask(
      remainingVars = retainedVariables,
      domainSizes = domains,
      factorVariables = (inputFactors ++ fixedFactors.map(_.variables)).toArray,
      ring)

    val values: Seq[Array[Double]] = fixedFactors.map(_.values)

    if(fixedFactors.isEmpty)
      (ins: IndexedSeq[Array[Double]], result: Array[Double]) => {
        spTask.sumProductNormalize(ins,result)
      }
    else
      (ins: IndexedSeq[Array[Double]], result: Array[Double]) => {
        spTask.sumProductNormalize(ins ++ values,result)
      }
  }

  case class V2F(v: Int, f: Int, vc: C) extends BPEdge {
    override type InEdge = BPEdge

    lazy val inputs: IndexedSeq[InEdge] =
      for(of <- problem.factorIdxOfVariable(v) if of != f)
      yield createF2VMessage(of, v, vc)

    //The enforcement of the condition
    lazy val conditionedBelief: Factor = Factor.generalDeterministicMaxEntropy(
      Array(v),
      Map(v -> scheme.allowedValuesUnderCondition(v, vc)),
      problem.domains,
      problem.ring)

    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit = constructSPTask(
      inputFactors = inputs.map(f2v => Array(f2v.v)),
      retainedVariables = Array(v),
      fixedFactors = if(scheme.isVariableEffectedByCondition(v,vc)) Seq(conditionedBelief) else Seq()
    )

    override def dotNodeOption: Seq[String] = super.dotNodeOption :+ "color = red"

    override def drawColor: String = "red"
  }

  case class F2V(f: Int, v: Int, fc: C) extends BPEdge {
    override type InEdge = V2F

    lazy val inputs: IndexedSeq[InEdge] =
      for(ov <- problem.factors(f).variables if ov != v)
      yield V2F(ov,f,scheme.superConditionOf(fc, Set(ov)))

    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit = constructSPTask(
      inputFactors = inputs.map(x => Array(x.v)),
      retainedVariables = Array(v),
      fixedFactors = Seq(problem.factors(f)))

    override def drawColor: String = "blue"
  }

  case class F2VAgg(f: Int, v: Int, vc: C) extends BPEdge {
    //this takes both F2V, as well as the conditional condition distribution
    override type InEdge = LcbpMessage

    lazy val subconditions: IndexedSeq[C] = scheme.subConditionsOf(vc, problem.factors(f).variables.toSet).toIndexedSeq

    lazy val inputs: IndexedSeq[InEdge] = subconditions.map(ccp(_,vc)) ++ subconditions.map(F2V(f,v,_))

    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit = {
      (ins: IndexedSeq[LcbpMessage#TOut], result: Array[Double]) =>
      val weights: Array[Double] = ins.take(subconditions.size).map(_.asInstanceOf[DoubleEdge#TOut].value)(collection.breakOut)
      val messages = ins.drop(subconditions.size).map(_.asInstanceOf[F2V#TOut])
        val myResult = linearCombination(weights,messages)
        System.arraycopy(myResult,0,result,0,myResult.length)
    }

    override def drawColor: String = "blue3"
  }

  /** Either build an aggregator or directly connect. */
  def createF2VMessage(f: Int, v: Int, vc: C): BPEdge =
    if(scheme.subConditionsOf(vc, problem.factors(f).variables.toSet).size == 1)
      F2V(f,v,vc)
    else
      F2VAgg(f,v,vc)

  /** Conditioned variable belief. */
  case class VBel(v: Int, vc: C) extends BaseFactorEdge{
    override type InEdge = BPEdge

    val variables: Array[Int] = Array(v)

    lazy val inputs: IndexedSeq[InEdge] = problem.factorIdxOfVariable(v).map(createF2VMessage(_,v,vc))

    //The enforcement of the condition
    lazy val conditionedBelief: Factor = Factor.generalDeterministicMaxEntropy(
      Array(v),
      Map(v -> scheme.allowedValuesUnderCondition(v, vc)),
      problem.domains,
      problem.ring)

    /** Just multiply the incoming messages. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit =
      constructSPTask(inputFactors = inputs.map(e => Array(e.v)),
        retainedVariables = variables,
        fixedFactors = Seq(conditionedBelief))
  }

  /** Conditioned factor belief. */
  case class FBel(f: Int, fc: C) extends BaseFactorEdge{
    override def variables: Array[Int] = problem.factors(f).variables

    override type InEdge = V2F

    lazy val inputs: IndexedSeq[InEdge] = variables.map(v => V2F(v,f,scheme.superConditionOf(fc,Set(v))))

    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[TOut], TOut) => Unit = constructSPTask(
      inputFactors = inputs.map(x => Array(x.v)),
      retainedVariables = variables,
      fixedFactors = Seq(problem.factors(f)))

  }
  
  /** The contribution the the average energy from this variable.
    * This is just the entropy of the conditioned variable belief
    * to the power of one minus degree. */
  case class FVariable(v: Int, vc: C) extends DoubleEdge{
    override type InEdge = VBel

    lazy val inputs: IndexedSeq[InEdge] = IndexedSeq(VBel(v,vc))

    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit = { (vbel,result) =>
      result.value = problem.ring.entropy(vbel(0)) * (1 - problem.degreeOfVariable(v))
    }

    override def drawColor: String = "coral"
  }

  case class FFactor(f: Int, fc: C) extends DoubleEdge{
    override type InEdge = FBel

    lazy val inputs: IndexedSeq[InEdge] = IndexedSeq(FBel(f,fc))
    
    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[InEdge#TOut], DoubleRef) => Unit = {(fbel,result) =>
      val dist: Array[Double] = fbel(0)
      val energy = problem.ring.entropy(dist) + problem.ring.logExpectation(dist,problem.factors(f).values)
      result.value = energy
    }

    override def drawColor: String = "aquamarine"
  }


  /** This must return a double-valued edge that computes the probability of condition `fc` given condition `vc`. */
  def ccp(fc: C, vc: C): DoubleEdge
  /** This must return a double-valued edge that computes the partition function of the meta problem. */
  def cdLogZ: DoubleEdge
}


