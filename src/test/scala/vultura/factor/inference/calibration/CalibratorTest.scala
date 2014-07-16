package vultura.factor.inference.calibration

import org.specs2.Specification
import org.specs2.specification.Fragments

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class CalibratorTest extends Specification {
  //averaging over boxed doubles
  case class AvgEdge(out: Int, deps: Int => IndexedSeq[Int]) extends CEdge {
    type D = java.lang.Double
    type TOut = D
    type ETIn = AvgEdge
    override def compute: (IndexedSeq[java.lang.Double]) => java.lang.Double = inDoubles =>
      inDoubles.map(_.doubleValue).sum / inDoubles.size
    override def input: IndexedSeq[AvgEdge] = deps(out).map(AvgEdge(_,deps))
    override def diff(r1: D, r2: D): Double = math.abs(r1 - r2)
    override def create: D = out.toDouble
  }

  def allAverage(nodeInts: Set[Int]): Set[CEdge] = {
    val map = (for{sink <- nodeInts; sources = nodeInts.filterNot(_ == sink)} yield sink -> sources.toIndexedSeq).toMap
    for(sink <- nodeInts) yield AvgEdge(sink, map)
  }

  override def is: Fragments =
    "average over 3 number nodes 1,2,3" !
      (new Calibrator(allAverage(Set(1,2,3))).isCalibrated must beTrue)
}

/** Check how it works with heterogeneous variables. More or less for checking typing. */
class CalibratorTest2 extends Specification {
  case class AToDEdge(out: Int, m: Int => IndexedSeq[Int]) extends CEdge {
    type ETIn = DToAEdge
    type TOut = java.lang.Double
    override def compute: (IndexedSeq[Array[Double]]) => java.lang.Double = inDoubles =>
      inDoubles.map(_.head).sum / inDoubles.size
    override def input: IndexedSeq[DToAEdge] = m(out).map(DToAEdge(_,m))

    /** @return the change between two values of this node. Zero means no change, lower means less change. */
    override def diff(r1: TOut, r2: TOut): Double =  math.abs(r1 - r2)
    /** Create a (mutable???) representation of the initial value of this node. */
    override def create: TOut = out.toDouble
  }
  case class DToAEdge(out: Int, m: Int => IndexedSeq[Int]) extends CEdge {
    type ETIn = AToDEdge
    type TOut = Array[Double]
    type R = TOut

    override def compute: (IndexedSeq[java.lang.Double]) => Array[Double] = inDoubles =>
      Array(inDoubles.map(_.doubleValue).sum / inDoubles.size)
    override def input: IndexedSeq[AToDEdge] = m(out).map(AToDEdge(_,m))
    override def diff(r1: R, r2: R): Double = math.abs(r1(0) - r2(0))
    override def create: R = Array.fill(1)(out)
    override def printValue(v: R): String = "AD(" + v.mkString(",") + ")"
  }

  def eachOther(nodeInts: Set[Int]): Set[CEdge] = {
    require(nodeInts.forall(i => i >= 0 && i < 10))
    val map = (for{i <- nodeInts; kv <- Seq(i -> IndexedSeq(i + 10), i + 10 -> IndexedSeq(i))} yield kv).toMap
    for(i <- nodeInts; e <- Seq(AToDEdge(i,map),DToAEdge(i+10,map))) yield e
  }

  override def is: Fragments =
    {
      "average over 3 number nodes 1,2,3" !
        {
          val calibrator: Calibrator = new Calibrator(eachOther(Set(1,2,3)))
          println(calibrator.valuesString)
          calibrator.isCalibrated must beTrue
        }
    }
}
