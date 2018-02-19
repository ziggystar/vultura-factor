package vultura.factor.inference.calibration

import vultura.factor.{Problem, Ring}

/** Stores just the raw message values of a BP run.
  * This class should not retain any references to other classes, and thus avoids memory leaks.
  */
class StoredResult(val problem: Problem, messages: BPResult#Message => Array[Double]) extends BPResult {

  override def ring: Ring[Double] = problem.ring

  val msgF2V: Array[Array[Array[Double]]] =
    problem.scopeOfFactor.zipWithIndex.map{case (scope,fi) => scope.map(v => new Array[Double](problem.domains(v)))}
  val msgV2F: Array[Array[Array[Double]]] =
    problem.scopeOfFactor.zipWithIndex.map{case (scope,fi) => scope.map(v => new Array[Double](problem.domains(v)))}

  {//copy stuff; this is not functional to avoid closing on `v2f` and `f2v`; and for performance
  var fi = 0
    while(fi < problem.numFactors){
      val scope = problem.scopeOfFactor(fi)
      var vii = 0
      while(vii < scope.length){
        val vi = scope(vii)

        msgF2V(fi)(vii) = messages(F2VMsg(fi, vi))
        msgV2F(fi)(vii) = messages(V2FMsg(vi, fi))

        vii += 1
      }
      fi += 1
    }
  }

  override def rawMessageValue(m: Message): Array[Double] = m match {
    case V2FMsg(vi,fi) => msgV2F(fi)(m.varIndexInFactorScope)
    case F2VMsg(fi,vi) => msgF2V(fi)(m.varIndexInFactorScope)
  }
}
