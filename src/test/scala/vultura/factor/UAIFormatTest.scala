package vultura.factor

import org.specs2._

class UAIFormatTest extends Specification with FactorMatchers {
  val p1 = vultura.factor.generators.grid(2,2,3)
  def is = s2"""
              serialize/deserialize 2x2 grid with |D| = 3: ${serializeDeserializeUai(p1)}
  """

  def serializeDeserializeUai(p: Problem) =
    forall(Problem.fromUaiString(p.uaiString).factors.zip(p.factors)){
      case (fr,original) => fr must beSimilarTo(original,1e-9)
    }
}
