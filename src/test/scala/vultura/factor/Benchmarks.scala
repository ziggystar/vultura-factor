package vultura.factor

import java.lang.management.{ThreadMXBean, ManagementFactory}

import scala.language.reflectiveCalls
import scala.util.Random
import vultura.factor
import vultura.util.stats._

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
object Benchmarks {

  case class SPTask(problem: Problem, retain: Array[Int], ring: Ring[Double], name: String)

  //one inner crossing of a grid problem
  val grids = Seq(
    "grid-D2" -> generators.grid(3,3,2,factor.generators.expGauss(0.3),new Random(0)).filter(_.variables.contains(4)),
    "grid-D4" -> generators.grid(3,3,4,factor.generators.expGauss(0.3),new Random(0)).filter(_.variables.contains(4)),
    "grid-D6" -> generators.grid(3,3,6,factor.generators.expGauss(0.3),new Random(0)).filter(_.variables.contains(4))
  )

  def ringName(r: Ring[Double]): String = if(r == NormalD) "n" else if(r == LogD) "l" else sys.error("unknown ring")
  def partitionTask(p: Problem, name: String, ring: Ring[Double]): SPTask = SPTask(p, Array(),ring,name + s":part:${ringName(ring)}")
  def flattenTask(p: Problem, name: String, ring: Ring[Double]): SPTask = SPTask(p,p.variables.toArray,ring,name + s":flat:${ringName(ring)}")

  def runTaskClassic(task: SPTask)(times: Int = 100): Unit = {
    val numValues = Factor.mapMultiply(task.retain,task.problem.domains)
    val values = new Array[Double](numValues)
    val factors = task.problem.factors
    val fvars: Array[Array[Int]] = factors.map(_.variables)(collection.breakOut)
    val fVals: Array[Array[Double]] = factors.map(_.values)(collection.breakOut): Array[Array[Double]]

    var i = 0
    while(i < times){
      Factor.sumProduct(task.retain, task.problem.domains, fvars, fVals, task.ring,values)
      i += 1
    }
  }

  def runTaskCompiled(task: SPTask)(times: Int = 100): Unit = {
    val numValues = Factor.mapMultiply(task.retain,task.problem.domains)
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


  def benchmark(task: () => Unit, name: String = "anonymous benchmark", repeats: Int = 5, warmup: Boolean = true): Unit = {
    if(warmup) this.warmup(task)
    val times = Seq.fill(repeats)(measure(task()))
    println(f"$name\tmean: ${times.mean}%.6fs\tsd: ${times.sd}%.6f")
  }

  def warmup(task: () => Unit): Unit = {
    var times: Seq[Double] = null
    val begin = System.nanoTime()
    do{
      times = Seq.fill(5)(measure(task()))
    } while(times.sd/times.mean > 0.02 && System.nanoTime() - begin < 5e9)
  }

  def measure(task: => Unit): Double = {
    val start = System.nanoTime()
    task
    (System.nanoTime() - start) * 1e-9
  }

  def times(n: Int)(task: () => Unit): () => Unit = () => {
    var i = 0
    while(i < n){
      task()
      i += 1
    }
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

  def printCPUTime[A](f: => A, label: String = ""): A = {
    val bean: ThreadMXBean = ManagementFactory.getThreadMXBean
    if(bean.isCurrentThreadCpuTimeSupported){
      val now = bean.getCurrentThreadCpuTime
      val r = f
      val t = (bean.getCurrentThreadCpuTime - now).toDouble * 1e-9
      println(s"$label took ${t}s")
      r
    } else {
      println("no thread cpu time support")
      f
    }
  }
}
