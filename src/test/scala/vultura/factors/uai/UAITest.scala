package vultura.factors.uai

import org.specs2._
import specification.Fragments
import java.io.File

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 26.09.12
 */

class UAITest extends Specification {
  def is: Fragments =
    "parse file without exception" ! (parseUAIMarkovFromFile(new File("test-res/uai/examples/GEOM30a_3.uai")) must not(throwAn[Exception]))
}
