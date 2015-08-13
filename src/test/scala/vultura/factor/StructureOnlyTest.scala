package vultura.factor

import org.specs2.Specification

class StructureOnlyTest extends Specification {
  override def is =
    StructureOnly(Array(2, 2), Array(Array(0, 1))) === StructureOnly(Array(2, 2), Array(Array(0, 1)))
}
