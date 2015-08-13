package vultura.factor.inference.conditioned.lcbp

import org.specs2.Specification
import vultura.factor.LogD
import vultura.factor.generators._

/**
 * Created by thomas on 23.09.14.
 */
class LcbpMetaBPTest extends Specification {
  override def is =
  "meta structure must not have correct domain sizes" !
    (new LcbpMetaBP(FactoredScheme.fromInfluenceMap(grid(2,2),Map(0 -> Set(0,1,2,3)))).metaStructure.domains.deep === Seq(2)) ^
  "meta structure must not have correct factor structure" !
    (new LcbpMetaBP(FactoredScheme.fromInfluenceMap(grid(2,2),Map(0 -> Set(0,1,2,3)))).metaStructure.scopeOfFactor.deep === Seq(Seq(0))) ^
  "if remainders after conditioning are trees, result must be exact" ! {
    val p = grid(2,2)
    val scheme = FactoredScheme.fromInfluenceMap(p, Map(0 -> Set(0,1,2,3)))
    val lcbpMetaBP: LcbpMetaBP = new LcbpMetaBP(scheme)
    (lcbpMetaBP.calibrator.isConverged.aka("converged") must beTrue) and (lcbpMetaBP.logZ must beCloseTo(p.logZ,1e-6))
  } ^
  "must yield same result as exact LCBP for tree-shaped meta problem" ^
    /* 3x3 grid; 0: conditioned by 0; x conditioned by 0,8; 8: conditioned by 8
    0 0 .
    0 x 8
    . 8 8
     */
    "on 3x3 grid" ! cmpExactLcbp(FactoredScheme.fromInfluenceMap(grid(3, 3),Map(0->Set(0,1,3,4),8->Set(8,7,5,4)))) ^
    "on 3x1 grid" ! cmpExactLcbp(FactoredScheme.fromInfluenceMap(grid(3,1), Map(0->Set(0,1),2->Set(1,2)))) ^
    "on 3x2 grid (simplified)" ! cmpExactLcbp(FactoredScheme.fromInfluenceMap(grid(3,2).simplify, Map(0->Set(0,1,3,4),5->Set(1,2,4,5)))) ^
    "on 3x2 grid" ! cmpExactLcbp(FactoredScheme.fromInfluenceMap(grid(3,2), Map(0->Set(0,1,3,4),5->Set(1,2,4,5)))) ^
   p^
   "must yield finite result when using log ring" !
     cmpExactLcbp(FactoredScheme.fromInfluenceMap(grid(3,1).toRing(LogD), Map(0->Set(0,1),2->Set(1,2))), useDeltaTerm = true)

  def cmpExactLcbp(scheme: FactoredScheme, printDot: Option[String] = None, useDeltaTerm: Boolean = false) = {
    val lcbp_exact = new LCBPGeneral(scheme)//new LCBP(scheme.problem,scheme.toGScheme, tol=1e-12, maxIterations = 1000000)
    val lcbp_bp = new LcbpMetaBP(scheme, tol=1e-15, maxUpdates = 1000000, useDeltaTerm = useDeltaTerm)
    (lcbp_bp.metaStructure.isTree.aka("meta problem is a tree") must beTrue) and
      (lcbp_exact.calibrator.isConverged.aka("lcbp exact converged") must beTrue) and
      (lcbp_bp.calibrator.isConverged.aka("lcbp bp converged") must beTrue) and
      (lcbp_bp.logZ.aka("lcbp-bp logZ") must beCloseTo(lcbp_exact.logZ,1e-6))
  }
}
