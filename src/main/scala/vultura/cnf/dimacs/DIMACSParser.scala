package vultura.cnf.dimacs

import java.io.File

/**
 * Reads a SAT instance from a text file in the cnf format of the DIMACS format.
 *
 * The format is specified in the following file:
 * http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/satformat.ps
 *
 * User: Thomas Geier
 * Date: 02.05.11 */

object DIMACSParser {
  def readFile(file: String): DIMACSInstance = readFile(new File(file))
  def readFile(file: File): DIMACSInstance = parse(scala.io.Source.fromFile(file).getLines())

  def parse(lines: Iterator[String]): DIMACSInstance = {
    val nonCommentLines = lines.filterNot(l => l.headOption == Some('%') || l.isEmpty)

    val description = nonCommentLines.takeWhile(_.headOption == Some('c')).map(_.drop(1)).mkString("\n")

    val problemSpec = nonCommentLines.takeWhile(_.headOption == Some('p')).map(_.drop(1)).mkString(" ")

    val splitSpec = problemSpec.split(" ").filter(!_.isEmpty)

    assert(splitSpec.size == 3, "specification must have three entries: " + splitSpec.mkString(" "))
    assert(splitSpec(0).contains("cnf"), "format description must contain 'cnf'")

    val numVariables = splitSpec(1).toInt
    val numClauses = splitSpec(2).toInt

    val numberSequence = nonCommentLines
      .filter(!_.headOption.exists(Seq('p', 'c').contains)) //drop comment and spec lines
      .mkString(" ") //join the lines
      .split(" ").filter(!_.isEmpty)
      .map(_.toInt) //turn into a seq of Int

    def splitAt[A](in: List[A], at: A, acc: List[List[A]] = List(Nil)): List[List[A]] = {
      in match {
        case Nil => acc
        case x :: tail if x == at => splitAt(tail, at, Nil :: acc)
        case x :: tail => splitAt(tail, at, (x :: acc.head) :: acc.tail)
      }
    }

    val clauses = splitAt(numberSequence.toList, 0).reverse.map(_.reverse)
      .filter(_.nonEmpty)
      .map {
      vars =>
        val (pos, neg) = vars
          .partition(_ > 0)
        DimacsClause(pos, neg.map(-1 * _))
    }


    assert(numClauses == clauses.size, "given number of clauses must equal the number of parsed clauses")

    DIMACSInstance(description, numVariables, clauses)
  }
}



