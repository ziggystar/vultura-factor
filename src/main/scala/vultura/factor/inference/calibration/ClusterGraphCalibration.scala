package vultura.factor.inference.calibration

import vultura.factor.inference.MargParI
import vultura.factor.{Ring, SumProductTask, Factor, Problem}

import scala.collection.mutable

//case class ProblemStructure(variablesOfFactor: Array[Array[Int]], domains: Array[Int])
//
//case class CGCalibrator(structure: ProblemStructure,
//                        ring: Ring[Double],
//                        defaultTolerance: Double = 1e-12,
//                        defaultMaxUpdates: Long = 10000) extends MargParI {
//  def calibrate(factorValues: Array[Array[Double]],
//                tol: Int = defaultTolerance,
//                maxUpdates: Long = defaultMaxUpdates): Unit = ???
//}
case class ClusterGraphCalibration(problem: Problem) {

  sealed trait BPMessage extends MEdge {self: Product =>
    def v: Int
    def f: Factor
    final type TOut = Array[Double]
    def create: TOut = new Array[Double](problem.domains(v))
    override def copy(t: TOut): TOut = t.clone()
  }

  case class V2F(v: Int, f: Factor) extends BPMessage {
    override type InEdge = F2V
    override def inputs: IndexedSeq[InEdge] = for(nf <- problem.factorsOfVariable(v) if nf != f) yield F2V(nf,v)
    def mCompute() = {
      //this must be lazy, otherwise inputs gets called indefinitely
      val spTask = SumProductTask(
        remainingVars = Array(v),
        domainSizes = problem.domains,
        inputs.map(f2v => Array(f2v.v))(collection.breakOut),
        problem.ring
      )
      (ins: IndexedSeq[Array[Double]], result: Array[Double]) => {
        spTask.sumProduct(ins,result)
        problem.ring.normalizeInplace(result)
      }
    }
  }

  case class F2V(f: Factor, v: Int) extends BPMessage {
    override type InEdge = V2F
    override def inputs: IndexedSeq[InEdge] = for(nv <- f.variables if nv != v) yield V2F(nv,f)
    override def mCompute() = {
      val spTask = SumProductTask(
        remainingVars = Array(v),
        domainSizes = problem.domains,
        (inputs.map(v2f => Array(v2f.v))(collection.breakOut) :+ f.variables)(collection.breakOut),
        problem.ring)
      val factorHolder = new mutable.ArraySeq[Array[Double]](f.variables.size)

      (ins: IndexedSeq[Array[Double]], result: Array[Double]) => {
        var i = 0
        while(i < ins.size){
          factorHolder(i) = ins(i)
          i += 1
        }
        factorHolder(i) = f.values

        spTask.sumProduct(factorHolder,result)
        problem.ring.normalizeInplace(result)
      }
    }
  }

  val edges: Iterable[BPMessage] = for{
    f <- problem.factors
    v <- f.variables
    edge <- Seq(F2V(f,v),V2F(v,f))
  } yield edge

  val maxEntInitializer = new EdgeValues[BPMessage] {
    override def hasEdge(e: BPMessage): Boolean = true
    override def edgeValue(e: BPMessage): e.type#TOut =  Factor.maxEntropy(Array(e.v),problem.domains,problem.ring).values
  }
}
