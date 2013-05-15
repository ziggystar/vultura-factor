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
object uai2cnf {
  class Config(args: Seq[String]) extends ScallopConf(args) {
    implicit val fileConverter: ValueConverter[File] = scallop.singleArgConverter(new File(_))
    implicit val inStream = scallop.singleArgConverter[InputStream](new FileInputStream(_))
    implicit val outStream = scallop.singleArgConverter[OutputStream](new FileOutputStream(_))

    banner("Conversion of markov networks in `uai` format to structurally equivalent SAT instances in `cnf` format. This " +
      "can be used to analyze treewidth with `quickbb`.")
    footer("this is the footer")
    version("uai2cnf 1.0")

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
    val shatter = opt[Boolean](
     "shatter",
      's',
      descr="shatter variables and create one variable for each possible assignment for old variables",
      default = Some(false)
    )
    val fileStat = opt[Boolean](
      "print-stat",
      'p',
      descr="print statistics of problem and exit"
    )
  }

  case class SingleValueFactor(variables: Array[Int],domains: Array[Array[Int]],assignment: Array[Int],value: Double,default: Double)
  implicit val svvTC = new SparseFactor[SingleValueFactor,Double]{
    def variables(f: SingleValueFactor): Array[Int] = f.variables
    def domains(f: SingleValueFactor): Array[Array[Int]] = f.domains
    def defaultValue(f: SingleValueFactor): Double = f.default
    def points(f: SingleValueFactor): Map[mutable.WrappedArray[Int], Double] = Map(wrapIntArray(f.assignment) -> f.value)
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
      outS.println(f"${vars.size}\t${inProblem.size}\t${domsizes.max}\t${domsizes.sum / domsizes.size.toDouble}%.2f\t${numZeros}%.2f\t${mmp}%.2f")
      System.exit(0)
    }

    def shatter(p: Seq[TableFactor[Double]]): Seq[Either[TableFactor[Double],SingleValueFactor]] = {
      val (vars,doms) = p.flatMap(f => f.variables.zip(f.domains.map(_.toSeq))).distinct.unzip
      assert(vars.size == vars.distinct.size, "differing domains for same variable")
      val oldVarDomMap: Map[Int, Array[Int]] = (vars zip doms.map(_.toArray)).toMap

      //map from (variable,value) pairs to new variable indices
      val newVars: Map[(Int, Int), Int] =
        (for ((v, d) <- vars zip doms; value <- d) yield (v, value))
          .zipWithIndex
          .map{case (k,v) => k -> (v+1)}
          .toMap
      def shatterFactor(f: TableFactor[Double]): IndexedSeq[SingleValueFactor] = {
        val mode = f.data.groupBy(identity).toSeq.sortBy(_._2.size).filter(_._1 != 0d).last._1
        f.cpi.collect{
          case assignment if(f.evaluate(assignment) != mode) =>
            SingleValueFactor(
              f.variables.zip(assignment).map(newVars),
              f.variables.map(_ => Array(0,1)),
              f.variables.map(_ => 1),
              f.evaluate(assignment),
              mode
            )
        }
      }
      def allDiffFactor(v: Int): TableFactor[Double] = TableFactor.fromFunction(
        oldVarDomMap(v).map(value => v -> value).map(newVars),
        Array.fill(oldVarDomMap(v).size)(Array(0,1)),
        assignment => if(assignment.sum == 1) 1d else 0d
      )

      vars.map(allDiffFactor).map(Left(_)) ++ p.flatMap(shatterFactor).map(Right(_))
    }

    def writeCNF[A : ({type L[X] = Factor[X,_]})#L](p: Seq[A]): String = {
      import vultura.factors._
      //map variables to their dimacs variable name
      val varMap: Map[Int, Int] = p.flatMap(variables(_)).distinct.toArray.zipWithIndex.map(t => (t._1,t._2 + 1)).toMap
      f"c this SAT instance represents the structure of a markov network read from ${config.input()}\n" +
      f"p cnf ${varMap.size} ${p.size}\n" +
      p.map(tf => variables(tf).map(varMap).mkString(" ") + " 0").mkString("\n")
    }

    val outString = (if(config.shatter()) writeCNF(shatter(inProblem)) else writeCNF(inProblem))

    //write out
    new PrintStream(config.output()).print(outString)
  }
}
