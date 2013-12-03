package vultura.fastfactors

/**
 * Contains example problems with some ground truths.
 */
object SampleProblems {
  class Example(val resourcePath: String, val logZ: Option[Double]){
    val filename: String = resourcePath.split("/").last
    lazy val problem: Problem = Problem.parseUAIProblem(ClassLoader.getSystemResourceAsStream(resourcePath)).right.get
  }

  //if there is a ground truth for log Z, then append it in log_10 base after a '}'
  val exampleProblems = """uai/examples/30markers.uai
                          |uai/examples/grid4x4.uai}44.4495
                          |uai/examples/deer_rescaled_0034.K10.F1.25.model.uai
                          |uai/examples/or_chain_10.fg.uai
                          |uai/examples/network.uai
                          |uai/examples/smokers_10.uai
                          |uai/examples/GEOM30a_3.uai
                          |uai/examples/grid3x3.uai}22.8274
                          |uai/examples/10_14_s.binary.uai
                          |uai/examples/Family2Dominant.1.5loci.uai
                          |uai/examples/grid2x2.uai}10.9241
                          |uai/examples/grid10x10.f2.wrap.uai
                          |uai/examples/foo.uai}6.91828""".stripMargin.split("\n").toSeq
  val examples: Seq[Example] = exampleProblems.map{ path =>
    val Pat = """([^}]*)}?(.*)""".r
    path match {
      case Pat(p,"") => new Example(p,None)
      case Pat(p,gt) => new Example(p,Some(gt.toDouble / math.log10(math.E)))
    }
  }
}
