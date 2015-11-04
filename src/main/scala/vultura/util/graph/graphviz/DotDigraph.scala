package vultura.util.graph.graphviz

import java.io.{File, FileOutputStream, PrintStream}

import vultura.util.SIIndex
import vultura.util.graph.BiSet

import scala.sys.process._

case class DotGraph[N,E](nodes: Iterable[N],
                         edges: Iterable[E],
                         nodeAttributes: Map[N,Seq[NAttr]] = Map[N,Seq[NAttr]](),
                         edgeAttributes: Map[E,Seq[EAttr]] = Map[E,Seq[EAttr]]()) {
  lazy val nodeIndex = new SIIndex[N](nodes)
  lazy val edgeIndex = new SIIndex[E](edges)

  def nodeID(n: N) = s"n${nodeIndex.forward(n)}"
  def edgeID(e: E) = s"e${edgeIndex.forward(e)}"

  def addNodeAttribute(attr: PartialFunction[N,NAttr]): DotGraph[N,E] = this.copy(
    nodeAttributes = nodeIndex.elements.map{ n =>
        n -> (nodeAttributes.getOrElse(n,Seq()) ++ attr.lift(n).map(Seq(_)).getOrElse(Seq()))
    }(collection.breakOut)
  )

  def addEdgeAttribute(attr: PartialFunction[E,EAttr]): DotGraph[N,E] = this.copy(
    edgeAttributes = edgeIndex.elements.map{ n =>
        n -> (edgeAttributes.getOrElse(n,Seq()) ++ attr.lift(n).map(Seq(_)).getOrElse(Seq()))
      }(collection.breakOut)
    )

  def labelNodes(labels: PartialFunction[N,String]): DotGraph[N,E] = addNodeAttribute(labels andThen (Label(_)))
  def labelEdges(labels: PartialFunction[E,String]): DotGraph[N,E] = addEdgeAttribute(labels andThen (Label(_)))

  def nodeString(n: N): String =
    s"\t${nodeID(n)} [${nodeAttributes.getOrElse(n, Seq()).map(_.dotString).mkString(",")}];"

  def renderDot(implicit dir: Dir[N,E]): String = Seq(
    s"${dir.header} automated {",
    nodeIndex.elements.map(nodeString).mkString("\n"),
    edgeIndex.elements
      .map(e => e -> dir.incidentNodes(e))
      .map{case (e,(n1,n2)) => dir.edgeString(nodeID(n1), nodeID(n2), edgeAttributes.getOrElse(e,Seq()))}
      .mkString("\n"),
    "}"
  ).mkString("\n")

  /** Render the graph dot->pdf. Producing `baseName.dot` and `baseName.pdf`*/
  def renderPDF(baseName: String)(implicit dir: Dir[N,E]): Unit = {
    val out = new PrintStream(new FileOutputStream(baseName + ".dot"))
    out.print(renderDot)
    out.close()
    (s"dot -Tpdf -K${dir.layout} $baseName.dot" #> new File(baseName + ".pdf")).!
  }
}

sealed trait Dir[N,E] {
  /** layout engine passed as option `-K`. */
  def layout: String
  def edgeString(nId1: String, nId2: String, attributes: Seq[Attr]): String
  def header: String
  def incidentNodes(e: E): (N,N)
}

object Dir {
  implicit def tupleDirectedInstance[N] = Directed[N,(N,N)](identity)
}
case class Undirected[N,E](f: E => BiSet[N], layout: String = "neato") extends Dir[N,E]{
  def header = "graph"
  override def incidentNodes(e: E): (N, N) = (f(e).e1,f(e).e2)
  override def edgeString(nId1: String, nId2: String, attributes: Seq[Attr]): String =
    s"$nId1 -- $nId2 [${attributes.map(_.dotString).mkString(",")}];"
}
case class Directed[N,E](f: E => (N,N), layout: String = "dot") extends Dir[N,E] {
  def header = "digraph"
  override def incidentNodes(e: E): (N, N) = f(e)
  override def edgeString(nId1: String, nId2: String, attributes: Seq[Attr]): String =
    s"$nId1 -> $nId2 [${attributes.map(_.dotString).mkString(",")}];"
}

