package vultura.util

import org.specs2._

class IndexTest extends Specification {
  val someCollection: Set[Char] = Set('a', 'b', 'c', 'd', 'b')
  val arrayIdx = new ArrayIndex(someCollection)
  val isIdx = new SIIndex(someCollection)
  val size = someCollection.toSet.size

  override def is =
    "size of AIdx" ! (arrayIdx.size === size) ^
      "size of ISIdx" ! (isIdx.size === size) ^
      "indices are correct" ! (arrayIdx.indices === (0 until size))
      "check identity law for arrayIdx" !
        someCollection.map((c: Char) => arrayIdx.backward(arrayIdx.forward(c)) === c).reduce(_ and _) ^
      "check identity law for isIdx" !
        someCollection.map((c: Char) => isIdx.backward(isIdx.forward(c)) === c).reduce(_ and _)

}
