package vultura.util.graph

import java.io.{File, FileOutputStream, PrintStream}

import scala.sys.process._

@deprecated("use vultura.graph.graphviz._ instead","23.0.0")
/** A directed graphviz graph. */
case class DotGraph[N](edges: Iterable[(N,N)],
                       additionalNodes: Iterable[N] = Set(),
                       graphName: String = "Dotted",
                       nodeOptions: Seq[N => String] = Seq(),
                       edgeOptions: Seq[((N,N)) => String] = Seq()){
  val nodes: Set[N] = (edges.flatMap(e => Seq(e._1,e._2)) ++ additionalNodes).toSet
  val nodeIDs: Map[N,String] = nodes.zipWithIndex.map{case (n,i) => n -> ("n" + i)}(collection.breakOut)
  def nodeStrings: Seq[String] = nodeIDs.map{case (n,id) => s"\t$id [${nodeOptions.map(_(n)).mkString(",")}]"}(collection.breakOut)
  def edgeStrings: Seq[String] = edges.map{case e@(src,dst) => s"${nodeIDs(src)} -> ${nodeIDs(dst)} [${edgeOptions.map(_.apply(e)).mkString(",")}]"}(collection.breakOut)
  def dotString: String = {
    s"""digraph $graphName {
       |${nodeStrings.mkString(";\n")}
       |${edgeStrings.mkString(";\n")}
       |}
    """.stripMargin
  }
  def nodeLabeled(l: N => String) = this.copy(nodeOptions=nodeOptions :+ ((n: N) => "label=\"" + l(n) + "\""))

  @deprecated("use writePDF instead")
  def toPDF(file: String): Unit = writePDF(file)

  def writePDF(file: String): Unit = {
    val out = new PrintStream(new FileOutputStream(file + ".dot"))
    out.print(dotString)
    out.close()
    (s"dot -Tpdf $file.dot" #> new File(file)).!
  }
}