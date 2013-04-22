package vultura.factors

import vultura.util.{DomainCPI, Measure}
import collection.mutable.WrappedArray
import java.io._

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 16.02.12
 */

package object uai {
  def generateUAIString[A,B](fs: Seq[A])(implicit ev: Factor[A,B], measure: Measure[B]): String = {
    require(
      fs.flatMap(domains(_)).map(_.toSeq).forall(d => d == (0 until d.size)),
      "all domains must be ranges beginning with zero")

    val (vs: Seq[Int],ds: Seq[WrappedArray[Int]]) =
      fs.flatMap(f => variables(f).zip(domains(f).map(wrapIntArray))).distinct.sortBy(_._1).unzip

    require(
      vs.size == vs.distinct.size,
      "no variable may have different domains between factors"
    )

    def generatePreambleFactorString(f: A): String = {
      variables(f).size + " " +
        variables(f).map(vs.indexOf(_)).mkString(" ")
    }

    def factorTable(f: A): String = {
      val cpi = new DomainCPI(domains(f), false)

      cpi.size + "\n " + cpi.iterator.map(evaluate(f,_)).map( value =>
        "%f".formatLocal(java.util.Locale.US, measure.weight(value))).mkString(" ")
    }

    val preamble = "MARKOV\n" +
      vs.size + "\n" +
      ds.map(_.size).mkString(" ") + "\n" +
      fs.size + "\n" +
      fs.map(generatePreambleFactorString).mkString("\n")

    val factorTables = fs.map(factorTable).mkString("\n")

    preamble + "\n\n" + factorTables
  }

  def parseUAIMarkovFromFile(file: File): Seq[TableFactor[Double]] = parseUAIMarkov(new FileInputStream(file))

  def parseUAIMarkov(in: InputStream): Seq[TableFactor[Double]] = {
      val reader = new BufferedReader(new InputStreamReader(in))
      val allLines = Iterator.continually(reader.readLine()).takeWhile(_ != null).mkString("\n")
      reader.close()
      val factors = parseUAIMarkov(allLines)
      factors
    }

  def parseUAIMarkov(description: String): Seq[TableFactor[Double]] = {
    //split on whitespace
    val tokens: Iterator[String] = "\\s+".r.split(description).toIterator

    //first token must be 'MARKOV'
    require(tokens.next().toUpperCase.matches("MARKOV"), "file must begin with 'MARKOV'")

    val numVars = tokens.next().toInt
    val domains = Array.fill(numVars)(tokens.next().toInt)
    val numFactors = tokens.next().toInt
    val factorVars = Seq.fill(numFactors){
      val nv = tokens.next().toInt
      Array.fill(nv)(tokens.next().toInt)
    }
    val factorValues = Seq.fill(numFactors){
      val nv = tokens.next().toInt
      Array.fill(nv)(tokens.next().toDouble)
    }

    (factorVars,factorValues).zipped.map{
      case (vars,vals) => TableFactor.fromTable(vars,vars.map(v => (0 until domains(v)).toArray),vals)
    }.toSeq
  }
}
