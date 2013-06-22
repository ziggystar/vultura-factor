package vultura.experiments

import org.specs2.Specification
import org.specs2.specification.Fragments

/**
 * @author Thomas Geier
 * @since 6/22/13
 */

class ExpTest extends Specification {
  def is: Fragments =
    "empty experiment must run and have empty header" ! (Exp().create._1 === Seq()) ^
    "empty experiment must have single empty row" ! (Exp().create._2.toList === List(List())) ^
    (Exp().flatMap(_ => Exp.values(1,2).addColumn("n",_.toString)).create._2.toSeq === List(List("1"),List("2"))) ^
    "expressions may only be evaluated once" ! {
      var i = 0
      val iterable = Iterator.continually({i += 1; i}).take(2)

      (for {
        i <- Exp.fromIterator(iterable).addColumn("n",_.toString)
      } yield Unit).create._2.toIndexedSeq === Seq(Seq("1"),Seq("2"))
    }
}