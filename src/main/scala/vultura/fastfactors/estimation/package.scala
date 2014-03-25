package vultura.fastfactors

import vultura.util.SSet

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
package object estimation {

  /** Learn the maximum likelihood estimate of some parameters of the given problem.
   *
   * @param problem NA values of factors will be learned. If tying is required, use the `targets` parameter.
   * @param data Training data. Each item in data is a partial assignment to the variables of problem.
   * @param targets Each set of features represents one parameter tied over the respective set of features.
   * @return
   */
  def mleEstimate(problem: Problem,
                  data: Iterable[Map[Int,Int]],
                  targets: IndexedSeq[Set[Feature]] = IndexedSeq()): (Problem, IndexedSeq[Double]) = {
    val scopes: SSet[Int] = new SSet(problem.factors.map(_.variables.toSet)(collection.breakOut))

    //TODO does this really matter?
    require(targets.flatten.forall(feature => !scopes.superSetsOf(feature.variables.toSet).isEmpty),
      "a feature scope is not a subset of an existing factor scope")

    //original problem is the problem before grounding out all data instances
    //ground problem is the problem after grounding out all data instances

    val numGroundings = data.size
    def groundToOriginal(groundVar: Var): Var = groundVar % problem.variables.size
    def originalToGround(v: Var, dataIndex: Int): Var = (dataIndex * problem.variables.size) + v

    val groundData: Map[Var,Val] = (for {
      (d, i) <- data.zipWithIndex
      (variable, value) <- d
    } yield originalToGround(variable, i) -> value)(collection.breakOut)

    val (nanFeatures, validFeatures) = Feature.extractFeaturesFromProblem(problem).partition(_._2.isNaN)
    
    //the features that are NA in the original problem and for which no tied groups exist
    val (learnFromNaNFeatures: Iterable[Set[Feature]], nolearnFeatures: Map[Feature, Double]) = {
      val tiedFeatures: Set[Feature] = targets.flatten.toSet
      val learn = nanFeatures.collect{
        case (f,value) if !tiedFeatures.contains(f) => Set(f)
      }

      val noLearn = validFeatures.filterNot(featureValueTuple => tiedFeatures.contains(featureValueTuple._1))

      (learn,noLearn)
    }
    
    val allOriginalTargets: IndexedSeq[Set[Feature]] = (targets ++ learnFromNaNFeatures).toIndexedSeq

    //make copies of a feature for each ground problem
    def groundFeature(f: Feature): Set[Feature] =
      (for(i <- 0 until numGroundings)
      yield f.copy(variables = f.variables.map(originalToGround(_,i))))(collection.breakOut)
    
    val groundTargets: IndexedSeq[Set[Feature]] = allOriginalTargets.map(_.flatMap(f => groundFeature(f)))

    val groundFixedFeatures: IndexedSeq[(Feature, Double)] = (for{
      (feature, value) <- nolearnFeatures
      gFeature <- groundFeature(feature)
    } yield gFeature -> value)(collection.breakOut)

    val task = LearningTask(
      Array.fill(numGroundings)(problem.domains).flatten,
      groundFixedFeatures,
      groundTargets,
      problem.ring
    )

    val estimator: ExactGradientAscent = ExactGradientAscent(task, groundData)

    val (estimate, loglikelihood) = estimator.estimate

    //build original problem with learned parameters
    val factors: IndexedSeq[FastFactor] = (for{
      (feature, value) <-  nolearnFeatures ++ (for((fs,v) <- allOriginalTargets.zip(estimate); f <- fs) yield (f,v))
    } yield feature.toFastFactor(problem.domains,problem.ring,value))(collection.breakOut)

    (Problem(factors,problem.domains,problem.ring).simplify, estimate)
  }
}
