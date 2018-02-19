package vultura.factor.inference.conditioned.lcbp

import vultura.factor._
import vultura.factor.inference.conditioned.Condition
import vultura.util.{ArrayIndex, CrossProductIndexer}

/** This trait exposes the meta problem by providing a [[ProblemStructure]] and edges
  * computing the factor values of the meta-factors. */
trait LcbpFactoredBase extends LcbpBase {

  /** Whether to apply the correction term. This influences the values of the factors. */
  def useDeltaTerm: Boolean
  override type ST <: FactoredScheme

  //meta variable index
  type MVI = Int
  //meta factor index
  type MFI = Int

  val var2metaVar: ArrayIndex[Int] = new ArrayIndex[Int](scheme.allConditioners.toSeq.sorted)

  def metaConditionersOf(vs: Set[Int]): Set[Int] = scheme.conditionersOf(vs).map(var2metaVar.forward)
  //using variable indices from meta problem, don't collect factors in super factors
  val metaScopes: Array[Array[Int]] =
    (problem.factors.map(f => metaConditionersOf(f.variables.toSet)) ++
      problem.variables.map(v => metaConditionersOf(Set(v))))
      .distinct
      .map(_.toArray.sorted)(collection.breakOut)
  val metaStructure: ProblemStructure =
    StructureOnly((0 until var2metaVar.size).map(problem.domains)(collection.breakOut),metaScopes)

  def metaRing: Ring[Double] = LogD
  def conditionVarToMetaVar(v: Int): MVI = var2metaVar.forward(v)
  def conditionToMetaCondition(c: Condition): Condition = c.map{case (k,v) => conditionVarToMetaVar(k) -> v}
  def containingMetaClique(mvs: Iterable[MVI]): MFI =
    metaScopes.zipWithIndex.find{case(scope,_) => mvs.toSet.subsetOf(scope.toSet)}.get._2

  def metaFactorEdge(fi: MFI): MetaFactorEdge = MetaFactor(fi)

  //below follows support code

  trait MetaFactorEdge extends FactorEdge{self: Product =>
    val domains: Array[Int] = metaStructure.domains
  }

  /** Return the index of a meta factor that covers the conditioners of the given variable set.
    * @param baseVars Usually this is a single variable or a factor scope.
    * @return Index of a meta factor.
    */
  def metaFactorIndexOfBaseConditioners(baseVars: Set[Int]): MFI = {
    val metaScope = metaConditionersOf(baseVars)
    metaScopes.zipWithIndex.find{case (mscope, mfi) => mscope.toSet == metaScope}.get._2
  }

  //in which meta factor are the variable contributions collected
  //note that we already pool factors, and variable contributions are aggregated in some factor contribution
  val variableContributionAssignment: Array[MFI] =
    problem.variables.map(v => metaFactorIndexOfBaseConditioners(Set(v)))(collection.breakOut)

  //in which meta factor are the factor contributions collected
  val factorContributionAssignment: Array[MFI] =
    problem.scopeOfFactor.map(scope => metaFactorIndexOfBaseConditioners(scope.toSet))

  case class MetaFactor(mfi: MFI) extends MetaFactorEdge {
    override type InEdge = DoubleEdge
    //those are the meta variables
    override def variables: Array[Int] = metaScopes(mfi)

    //the last entry is the index of the condition, thus the pointer into the values array of the built factor
    val inputLookup: IndexedSeq[(DoubleEdge, Int)] = {
      val baseVariables: Array[Var] = variables.map(var2metaVar.backward)

      //collect all members of factor- and variableContributionAssignment that are assigned to this MetaFactor
      //(with index mfi).
      //If you wonder whether the FVariable get used too often (since they will be accessed with factor conditions,
      //instead of their usual variable conditions), this is correct and basically pre-multiplies the variable
      //contribution into the according factor-contributions
      val contributions: Array[Condition => DoubleEdge] =
        variableContributionAssignment.zipWithIndex
          .collect{ case (assign: MFI, baseVI) if assign == mfi => FVariable(baseVI, _: Condition)} ++
          factorContributionAssignment.zipWithIndex
            .collect{ case (assign: MFI, baseFI) if assign == mfi => FFactor(baseFI, _: Condition)} ++
          (
            if (useDeltaTerm) factorContributionAssignment.zipWithIndex
              .collect{ case (assign: MFI, baseFI) if assign == mfi => DeltaTerm(baseFI, _: Condition)}
            else Seq()
          )


      for {
        (assignment, idx) <- new CrossProductIndexer(baseVariables.map(problem.domains)).zipWithIndex
        condition = baseVariables.zip(assignment).toMap //this will be the condition argument to the base messages
        contrib <- contributions
      } yield (contrib(condition),idx)
    }

    //inputs to a `MetaFactor` are all variable and factor contributions that have
    //the matching conditioner set under all possible conditions
    override def inputs: IndexedSeq[InEdge] = inputLookup.map(_._1)

    /** These computations don't have to be thread-safe. */
    override def mCompute(): (IndexedSeq[DoubleRef], TOut) => Unit = { (ins,result) =>
      for(i <- result.indices) result(i) = metaRing.one

      for( ((_,idx),DoubleRef(x)) <- inputLookup zip ins) {
        result(idx) = metaRing.prod(result(idx), x)
      }
    }
  }
}
