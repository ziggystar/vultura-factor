package vultura.tools

import org.rogach.scallop.{ValueConverter, ScallopConf}
import java.io._
import org.rogach.scallop
import vultura.factors._
import scala.Some
import vultura.factors.uai
import scala.collection.mutable

/**
 * Convert a uai markov problem to a structurally equivalent SAT instance in *.cnf format.
 */
object uai2uai {
  class Config(args: Seq[String]) extends ScallopConf(args) {
    implicit val fileConverter: ValueConverter[File] = scallop.singleArgConverter(new File(_))
    implicit val inStream = scallop.singleArgConverter[InputStream](new FileInputStream(_))
    implicit val outStream = scallop.singleArgConverter[OutputStream](new FileOutputStream(_))

    banner("Perform actions onmarkov networks in `uai` format to structurally equivalent SAT instances in `cnf` format.")
    footer("this is the footer")
    version("uai2uai 1.0")

    val input = opt[InputStream](
      name="input",
      short='i',
      descr="file to read from (uai-format); stdin if not given",
      default=Some(System.in)
    )
    val output = opt[OutputStream](
      name="output",
      short='o',
      descr="file to write cnf to; stdout if not given",
      default=Some(System.out)
    )
    val task = trailArg[String](
      name = "task",
      descr = "select for task [compress]",
      validate = str => Seq("compress").contains(str.toLowerCase),
      default = Some("compress")
    )
    val fileStat = opt[Boolean](
      "print-stat",
      'p',
      descr="print statistics for problem and exit"
    )
  }

  def main(args: Array[String]) {
    val config = new Config(args)

    val inProblem: Seq[TableFactor[Double]] = uai.parseUAIMarkov(config.input())

    if(config.fileStat()){
      val (vars,doms) = inProblem.flatMap(f => f.variables.zip(f.domains.map(_.toSeq))).distinct.unzip
      assert(vars.size == vars.distinct.size, "differing domains for same variable")

      val domsizes = doms.map(_.size)
      val outS = new PrintStream(config.output())
      val numZeros = {
        val allvals = inProblem.flatMap(_.data)
        allvals.count(_ == 0d).toDouble / allvals.size
      }
      val mmp = {
        //for each factor, how often does the most frequent non-zero value appear
        val modeShare = inProblem.map{f =>
          f.data.groupBy(identity).filterNot(_._1 == 0d).toSeq.sortBy(_._2.size).last._2.size.toDouble / f.data.size
        }
        modeShare.sum / modeShare.size
      }
      outS.println("variables\tfactors\tlargestDomain\tmeanDomain\tdeterminism\tmeanModePercentage")
      outS.println(f"${vars.size}\t${inProblem.size}\t${domsizes.max}\t${domsizes.sum / domsizes.size.toDouble}%.2f\t$numZeros%.2f\t$mmp%.2f")
      System.exit(0)
    }
    val t = System.nanoTime()
    val outString: String = config.task() match {
      case "compress" =>
        val subsets: Map[Set[Int], Set[Int]] = findSubsets(inProblem.map(_.variables.toSet))
        println(f"reduce ${inProblem.size}, of which are ${subsets.keySet.size} distinct  to ${subsets.values.toSet.size}")
        subsets.toString
    }
    println((System.nanoTime() - t).toDouble * 1e-9)

    //write out
    new PrintStream(config.output()).print(outString)
  }

  def findSubsets(_sets: Seq[Set[Int]]): Map[Set[Int],Set[Int]] = {
    val plain: IndexedSeq[Set[Int]] = _sets.distinct.toIndexedSeq
    val vars = plain.flatten.distinct.toIndexedSeq.sorted
    val transposed: IndexedSeq[Set[Int]] = {
      val withIndex: Seq[(Set[Int], Int)] = plain.zipWithIndex
      vars.map(v => (for((vs,i) <- withIndex if vs contains v) yield i)(collection.breakOut):Set[Int])
    }
    val mapping = plain.zipWithIndex.foldLeft(Map[Int,Int]()){ case (acc,(ss,ci)) =>
      val subsumerIndices: Set[Int] = ss.map(transposed).reduce(_ intersect _)
      val subsumer = vultura.util.maxByMultiple(subsumerIndices.toSeq)(ci => plain(ci).size).sorted.last
      if(ci != subsumer) println(plain(ci) + " -> " + plain(subsumer))
      acc + (ci -> subsumer)
    }
    mapping.map{case (k,v) => plain(k) -> plain(v)}
  }
}
