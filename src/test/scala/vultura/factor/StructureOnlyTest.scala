package vultura.factor

import org.specs2.Specification
import org.specs2.specification.Fragments

class StructureOnlyTest extends Specification {
  override def is: Fragments =
    (StructureOnly(Array(2,2),Array(Array(0,1))) === StructureOnly(Array(2,2),Array(Array(0,1))))
}
