package vultura.fastfactors

import vultura.fastfactors
import scala.util.Random
import vultura.util._

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
object Benchmarks {

  case class SPTask(problem: Problem, retain: Array[Int], ring: RingZ[Double], name: String)

  //one inner crossing of a grid problem
  val grids = Seq(
    "grid-D2" -> generators.grid(3,3,2,fastfactors.generators.expGauss(0.3),new Random(0)).filter(_.variables.contains(4)),
    "grid-D4" -> generators.grid(3,3,4,fastfactors.generators.expGauss(0.3),new Random(0)).filter(_.variables.contains(4)),
    "grid-D6" -> generators.grid(3,3,6,fastfactors.generators.expGauss(0.3),new Random(0)).filter(_.variables.contains(4))
  )

  def ringName(r: RingZ[Double]): String = if(r == NormalD) "n" else if(r == LogD) "l" else sys.error("unknown ring")
  def partitionTask(p: Problem, name: String, ring: RingZ[Double]): SPTask = SPTask(p, Array(),ring,name + s":part:${ringName(ring)}")
  def flattenTask(p: Problem, name: String, ring: RingZ[Double]): SPTask = SPTask(p,p.variables.toArray,ring,name + s":flat:${ringName(ring)}")

  def runTaskClassic(task: SPTask)(times: Int = 100): Unit = {
    val numValues = FastFactor.mapMultiply(task.retain,task.problem.domains)
    val values = new Array[Double](numValues)
    val factors = task.problem.factors
    val fvars: Array[Array[Int]] = factors.map(_.variables)(collection.breakOut)
    val fVals: Array[Array[Double]] = factors.map(_.values)(collection.breakOut): Array[Array[Double]]

    var i = 0
    while(i < times){
      FastFactor.sumProduct(task.retain, task.problem.domains, fvars, fVals, task.ring,values)
      i += 1
    }
  }

  def runTaskCompiled(task: SPTask)(times: Int = 100): Unit = {
    val numValues = FastFactor.mapMultiply(task.retain,task.problem.domains)
    val values = new Array[Double](numValues)
    val factors = task.problem.factors
    val fvars: Array[Array[Int]] = factors.map(_.variables)(collection.breakOut)
    val fVals: Array[Array[Double]] = factors.map(_.values)(collection.breakOut): Array[Array[Double]]

    val compiledTask = SumProductTask(task.retain,task.problem.domains,fvars,task.ring)

    var i = 0
    while(i < times){
      compiledTask.sumProduct(fVals,values)
      i += 1
    }
  }


  def benchmark(task: () => Unit, name: String = "anonymous benchmark"): Unit = {
    warmup(task)
    val times = Seq.fill(5)(measure(task()))
    println(f"$name\tmean: ${times.mean}%.6fs\tsd: ${times.sd}%.6f")
  }

  def warmup(task: () => Unit): Unit = {
    var times: Seq[Double] = null
    do{
      times = Seq.fill(5)(measure(task()))
    } while(times.sd > 0.002)
  }

  def measure(task: => Unit): Double = {
    val start = System.nanoTime()
    task
    (System.nanoTime() - start) * 1e-9
  }

  def benchmarkTask(t: SPTask): Unit = {
    benchmark(() => runTaskClassic(t)(500),  "classic:\t" + t.name)
    benchmark(() => runTaskCompiled(t)(500), "compile:\t" + t.name)
  }

  val tasks = for{
    (name,prob) <- grids
    r <- Seq(NormalD,LogD)
    task <- Seq(partitionTask(prob,name,r),flattenTask(prob,name,r))
  } yield task

  def main(args: Array[String]) {
    for(t <- tasks){
      benchmarkTask(t)
    }
  }

}
