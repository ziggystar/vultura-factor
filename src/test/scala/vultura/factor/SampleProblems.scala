package vultura.factor

import java.io.StringReader

import scala.io.Source
import scala.util.Try

/**
 * Contains example problems with some ground truths.
 */
object SampleProblems {
  class Example(val resourcePath: String, val logZ: Option[Double]){
    val filename: String = resourcePath.split("/").last

    lazy val loadedProblem: Either[String, Problem] =
      for {
        resource <- Try(Source.fromResource(resourcePath)).toEither.left.map(_.toString)
        reader <- Try(resource.getLines().mkString("\n")).toEither.left.map(e => s"exception when reading lines: $e")
        problem <- Problem.parseUAIProblem(new StringReader(reader))
      } yield problem

    def problem: Problem = loadedProblem.fold(sys.error,identity)
  }

  //if there is a ground truth for log Z, then append it in log_10 base after a '}'
  val exampleProblems: Seq[String] = """problems/uai/examples/30markers.uai
                                       |problems/uai/examples/grid4x4.uai}44.4495
                                       |problems/uai/examples/deer_rescaled_0034.K10.F1.25.model.uai
                                       |problems/uai/examples/or_chain_10.fg.uai
                                       |problems/uai/examples/network.uai
                                       |problems/uai/examples/smokers_10.uai
                                       |problems/uai/examples/GEOM30a_3.uai
                                       |problems/uai/examples/grid3x3.uai}22.8274
                                       |problems/uai/examples/10_14_s.binary.uai
                                       |problems/uai/examples/Family2Dominant.1.5loci.uai
                                       |problems/uai/examples/grid2x2.uai}10.9241
                                       |problems/uai/examples/grid10x10.f2.wrap.uai
                                       |problems/uai/examples/foo.uai}6.91828
                                       |problems/uai/examples/smokers_10.uai""".stripMargin.split("\n").toSeq

  val examples: Seq[Example] = exampleProblems.map{ path =>
    val Pat = """([^}]*)}?(.*)""".r
    path match {
      case Pat(p,"") => new Example(p,None)
      case Pat(p,gt) => new Example(p,Some(gt.toDouble / math.log10(math.E)))
    }
  }

  def getExample(id: String): Example = examples.find(_.filename == id).get
}
