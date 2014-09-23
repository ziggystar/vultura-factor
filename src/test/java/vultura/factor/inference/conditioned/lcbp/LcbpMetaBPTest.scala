package vultura.factor.inference.conditioned.lcbp

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor.generators._

/**
 * Created by thomas on 23.09.14.
 */
class LcbpMetaBPTest extends Specification {
  override def is: Fragments =
  "meta structure must not have correct domain sizes" !
    (new LcbpMetaBP(FactoredScheme.fromInfluenceMap(grid(2,2),Map(0 -> Set(0,1,2,3)))).metaStructure.domains.deep === Seq(2)) ^
  "meta structure must not have correct factor structure" !
    (new LcbpMetaBP(FactoredScheme.fromInfluenceMap(grid(2,2),Map(0 -> Set(0,1,2,3)))).metaStructure.scopeOfFactor.deep === Seq(Seq(0))) ^
  "tree-conditioned must be exact" ! {
    val p = grid(2,2)
    val scheme = FactoredScheme.fromInfluenceMap(p, Map(0 -> Set(0,1,2,3)))
    val lcbpMetaBP: LcbpMetaBP = new LcbpMetaBP(scheme)
    lcbpMetaBP.calibrator.toDot.toPDF("lcbp-bp.pdf")
    new LCBPGeneral(scheme).calibrator.toDot.toPDF("lcbp-jt.pdf")
    (lcbpMetaBP.calibrator.isConverged must beTrue) and (lcbpMetaBP.logZ must beCloseTo(p.logZ,1e-6))
  }
}
