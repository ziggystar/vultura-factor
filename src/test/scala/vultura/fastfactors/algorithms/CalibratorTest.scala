package vultura.fastfactors.algorithms

import org.specs2.Specification
import org.specs2.specification.Fragments
import scala.reflect.ClassTag

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class CalibratorTest extends Specification {
  //averaging over boxed doubles
  case class DNode(i: Int) extends CNode{
    type R = java.lang.Double
    override def diff(r1: R, r2: R): Double = math.abs(r1 - r2)
    override def create: R = i.toDouble
  }
  case class AvgEdge(ins: IndexedSeq[Int], out: Int) extends CEdge {
    type TIn = java.lang.Double
    type TOut = java.lang.Double
    override def compute: (IndexedSeq[java.lang.Double]) => java.lang.Double = inDoubles =>
      inDoubles.map(_.doubleValue).sum / inDoubles.size
    override def output: DNode = DNode(out)
    override def input: IndexedSeq[DNode] = ins.map(DNode)
  }

  case class AllAverage(nodeInts: IndexedSeq[Int]) extends CalProb{
    override def edges = (for{
      sink <- nodeInts
      sources = nodeInts.filterNot(_ == sink)
    } yield AvgEdge(sources,sink))(collection.breakOut)
  }

  override def is: Fragments =
    "average over 3 number nodes 1,2,3" !
      (new Calibrator(AllAverage(IndexedSeq(1,2,3))).isCalibrated must beTrue)
}

/** Check how it works with heterogeneous variables. More or less for checking typing. */
class CalibratorTest2 extends Specification {
  case class DNode(i: Int) extends CNode {
    type R = java.lang.Double
    override def diff(r1: R, r2: R): Double = math.abs(r1 - r2)
    override def create: R = i.toDouble
  }
  case class ADNode(i: Int) extends CNode {
    type R = Array[Double]
    override def diff(r1: R, r2: R): Double = math.abs(r1(0) - r2(0))
    override def create: R = Array.fill(1)(i)

    override def printValue(v: R): String = "AD(" + v.mkString(",") + ")"
  }
  case class AToDEdge(ins: IndexedSeq[Int], out: Int) extends CEdge {
    type TIn = Array[Double]
    type TOut = java.lang.Double
    override def compute: (IndexedSeq[Array[Double]]) => java.lang.Double = inDoubles =>
      inDoubles.map(_.head).sum / inDoubles.size

    override def output: DNode = DNode(out)
    override def input: IndexedSeq[ADNode] = ins.map(ADNode)
  }
  case class DToAEdge(ins: IndexedSeq[Int], out: Int) extends CEdge {
    type TIn = java.lang.Double
    type TOut = Array[Double]

    override def compute: (IndexedSeq[java.lang.Double]) => Array[Double] = inDoubles =>
      Array(inDoubles.map(_.doubleValue).sum / inDoubles.size)

    override def output: ADNode = ADNode(out)
    override def input: IndexedSeq[DNode] = ins.map(DNode)
  }

  case class AToDProblem(from: IndexedSeq[Int]) extends CalProb {
    override def edges: Set[CEdge] = from.map(f => AToDEdge(IndexedSeq(f),0)).toSet ++ from.map(f => DToAEdge(IndexedSeq(0),f))
  }
  override def is: Fragments =
    {
      "average over 3 number nodes 1,2,3" !
        {
          val calibrator: Calibrator = new Calibrator(AToDProblem(IndexedSeq(1, 2, 3)))
          println(calibrator.valuesString)
          calibrator.isCalibrated must beTrue
        }
    }
}
