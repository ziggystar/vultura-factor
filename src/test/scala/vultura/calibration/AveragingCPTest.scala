package vultura.calibration

import org.specs2._
import org.specs2.specification.Fragments
import vultura.propagation._

class AveragingCPTest extends Specification {

  trait DoubleNode extends NodeAD {
    override type Type = Double

    override def construct: Array[Double] = new Array[Double](1)
    override def store(t: Double, r: Array[Double]): Unit = r(0) = t
    override def load(r: Array[Double]): Double = r(0)
  }

  case class Input(n: Int) extends DoubleNode
  case class Avg(ins: DoubleNode*) extends DoubleNode
  case class AvgID(id: Int) extends DoubleNode

  object AvgRule extends RuleAD[Avg,DoubleNode]{
    override def dependencies(v1: Avg): IndexedSeq[DoubleNode] = v1.ins.toIndexedSeq
    override def implementation(v1: Avg): ImplAD = new ImplAD {
      override def compute: (Array[Array[Double]], Array[Double]) => Unit = (ins,r) => r(0) = ins.map(_.head).sum / ins.size
    }
  }

  /** Create rules from a graph given as a map.
    * @param deps First in tuple are dependencies on [[Input]] nodes, second are dependencies on [[AvgID]] nodes.*/
  case class AvgGraph(deps: Map[Int,(Set[Int],Set[Int])]) extends RuleAD[AvgID,DoubleNode]{
    override def dependencies(v1: AvgID): IndexedSeq[DoubleNode] = {
      val (ins,avgs) = deps(v1.id)
      (ins.map(Input) ++ avgs.map(AvgID)).toIndexedSeq
    }
    override def implementation(v1: AvgID): ImplAD = new ImplAD {
      override def compute: (Array[Array[Double]], Array[Double]) => Unit = (ins,r) => r(0) = ins.map(_.head).sum / ins.size
    }
  }

  //one computed node is the average of two inputs
  val cp1: CP[DoubleNode,ImplAD] = CP(Seq(Avg(Input(1),Input(2)))) appendRule AvgRule
  val cp1Calib = new RoundRobinAD[DoubleNode](cp1,MaxDiff,Valuation.constant[DoubleNode](1d))

  val cp2: CP[DoubleNode,ImplAD] = CP(Seq(AvgID(1),AvgID(2))) appendRule AvgGraph(Map(1 -> (Set(1),Set(2)),2 -> (Set(2),Set(1))))
  val cp2Calib = new RoundRobinAD[DoubleNode](cp2,MaxDiff,Valuation.constant[DoubleNode](1d))

  case class InputParams(x: Map[Int,Double]) extends Valuation[DoubleNode] {
    override def apply(n: DoubleNode): n.Type = n match {
      case Input(k) => x(k)
      case _ => sys.error("unexpected node")
    }
  }

  override def is: Fragments =
    "calibration of average of 1 and 2 is 1.5" ! {
      val result = cp1Calib.calibrate(InputParams(Map(1 -> 1d,2 -> 2d)))
      result(Avg(Input(1),Input(2))) must beCloseTo(1.5,1e-9)
    } ^
    "cyclic calibration" ! {
      val result = cp2Calib.calibrate(InputParams(Map(1 -> 1d, 2 -> 2d)),maxDiff = 1e-6)
      (result(AvgID(1)) must beCloseTo(1.3333, 1e-3)) and
        (result(AvgID(2)) must beCloseTo(1.6666,1e-3)) and
        (result.isConverged must beTrue)
    }
}
