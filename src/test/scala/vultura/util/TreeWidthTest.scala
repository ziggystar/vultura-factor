package vultura.util

import org.specs2._
import specification.Fragments
import TreeWidth._
import vultura.factors.Factor
import vultura.cnf.{CNFasBIFun, CNF}
import scala.collection.immutable.IndexedSeq
import java.io.{FilenameFilter, File}
import scalaz.Tree

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 20.04.12
 */
class TreeWidthTest extends Specification {
  implicit val clauseAsFactor = CNFasBIFun.ClauseAsFun
  val allCNFs: Array[File] = new File("test-res/sat").listFiles(new FilenameFilter{
    def accept(dir: File, name: String): Boolean = name.endsWith("cnf")
  })

  /** Needed for input into the junciton tree algorithm: */
  def cliqueSeq[F](fs: Seq[F])(implicit evF: Factor[F,_]): Seq[(Set[Int],F)] = fs.map(f => (evF.variables(f).toSet,f))

  def is: Fragments =
    (treeWidth(List(Set(1,2),Set(2,3),Set(3,4),Set(1,4)),List(1,2,3,4)) === 2) ^
      (treeWidth(List(Set(1,2),Set(2,3)), List(1,2,3)) === 1) ^
    ((cnf:CNF) => checkJunctionTree(cnf.clauses) must beNone).forall(allCNFs.map(CNF.fromFile).toSeq)



  def checkJunctionTree[F](fs: Seq[F])(implicit evF: Factor[F,_]): Option[String] = {
    val cliques: IndexedSeq[(Set[Int], F)] = cliqueSeq(fs).toIndexedSeq
    val jt: Seq[Tree[(Set[Int], Seq[F])]] = minDegreeJunctionTreesCompressed[F](cliques)._1
    isJunctionTree[F](cliques, jt)
  }
}
