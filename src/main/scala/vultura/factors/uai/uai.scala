package vultura.factors

import vultura.util.{DomainCPI, Measure}
import collection.mutable.WrappedArray

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
}
