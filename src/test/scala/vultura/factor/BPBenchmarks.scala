package vultura.factor

import vultura.factor.inference.calibration.LBP

import scala.language.reflectiveCalls
import scala.util.Random
import vultura.factor.inference.{CalibrationProblem, RoundRobinCalibrator, BeliefPropagation}
import vultura.util.stats._


/**
 * Runs different implementations of BP on some problems.
 */

object BPBenchmarks {

  val problems: Seq[(String,Problem)] = Seq(
    "grid10x10x2" -> generators.grid(10,10,2,generators.expGauss(1),new Random(0)),
    "grid10x50x2" -> generators.grid(10,50,2,generators.expGauss(1),new Random(0)),
    "grid10x50x2/log" -> generators.grid(10,50,2,generators.expGauss(1),new Random(0)).toRing(LogD),
    "grid5x5x2" -> generators.grid(5,5,2,generators.expGauss(1),new Random(0)),
    "grid10x10x4" -> generators.grid(10,10,4,generators.expGauss(1),new Random(0)),
    "grid10x50x4" -> generators.grid(10,50,4,generators.expGauss(1),new Random(0)),
    "grid10x10x10" -> generators.grid(10,10,10,generators.expGauss(1),new Random(0)),
    "grid10x50x10" -> generators.grid(10,50,10,generators.expGauss(1),new Random(0))
  )

  val algs: Seq[(String, Problem => Any)] = Seq(
    "bp" ->     {p => new BeliefPropagation(p,new Random(0),1e-7,1000000).logZ},
//    "bp-cp" ->  {p => new RoundRobinCalibrator(CalibrationProblem.betheCalibrationProblem(p),10,1e-7,new Random(0))},
    "bp-cp2" -> {p => LBP.infer(p,tol = 1e-7, maxIterations = 1000000).logZ}
  )

  def main(args: Array[String]) {
    for{
      (dp,p) <- problems
      (da,a) <- algs
    }{
      benchmark(() => a(p),s"$dp/$da")
    }
  }
  
  def benchmark(task: () => Unit, name: String = "anonymous benchmark"): Unit = {
    val multiple = 5 //Iterator.iterate(1)(2 *).dropWhile(n => measure(timesTask(n)(task)()) < 1).next()
    val scaledTask = timesTask(multiple)(task)

    warmup(scaledTask)
    val measuredTimes = Seq.fill(5)(measure(scaledTask()))
    println(f"$name(*$multiple)\tmean: ${measuredTimes.mean}%.6fs\tsd: ${measuredTimes.sd}%.6f")
  }

  def warmup(task: () => Unit): Unit = {
    var times: Seq[Double] = null
    var startTime = System.nanoTime
    do{
      times = Seq.fill(5)(measure(task()))
    } while(times.sd > 0.05 && (System.nanoTime - startTime > 5e9))
  }

  def measure(task: => Unit): Double = {
    val start = System.nanoTime()
    task
    (System.nanoTime() - start) * 1e-9
  }
  def timesTask(n: Int)(task: () => Unit): () => Unit = () => {
    var i = 0
    while(i < n){
      task()
      i += 1
    }
  }  
}