package vultura.tools

import org.rogach.scallop.{ValueConverter, ScallopConf}
import java.io._
import org.rogach.scallop
import vultura.fastfactors._
import generators._
import vultura.fastfactors.algorithms._
import java.lang.management.ManagementFactory
import scala.util.Random
import vultura.fastfactors.Problem
import vultura.util._
/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/14/13
 */
object ictai13 {
  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

  class Config(args: Seq[String]) extends ScallopConf(args) {
    implicit val fileConverter: ValueConverter[File] = scallop.singleArgConverter(new File(_))
    implicit val inStream = scallop.singleArgConverter[InputStream](new FileInputStream(_))
    implicit val outStream = scallop.singleArgConverter[OutputStream](new FileOutputStream(_))

    banner("Run experiments on generated problems.")
    version("ictai13 1.0")

    val problemCfg = trailArg[String](
      name="problem-config",
      descr="configuration string for problem generator"
    )
    val problemSeed = opt[Long](
      name = "problem-seed",
      descr = "seed for problem generation",
      default = Some(0L)
    )
    val problemCount = opt[Int](
      name = "problem-count",
      short = 'n',
      descr = "generate this many problems",
      default = Some(1)
    )
    val algorithmCfg = trailArg[String](
      name = "algorithm-config",
      descr = "configuration string for algorithm"
    )
    val algorithmSeed = opt[Long](
      name = "algorithm-seed",
      default = Some(0L)
    )
    val algorithmRuns = opt[Int](
      name = "algorithm-runs",
      short = 'r',
      descr = "number of times the algorithm is run on the problem",
      default = Some(1)
    )
    val algorithmSteps = opt[Int](
      name = "algorithm-iterations",
      short = 'i',
      descr = "number of steps the algorithm shall perform",
      default = Some(10)
    )
    val dontPrintProblem = opt[Boolean](
      name = "no-print-problem",
      descr = "output each generated problem in uai format"
    )
  }

  /*
  Each output line contains:
  <problem-cfg> <problem-seed> <PR> <algorithm-cfg> <algorithm-seed> <PR estimate> <MAR KL> <MAR maxdiff> <MAR mean maxDiff> <iteration> <CPU time>
   */
  def main(args: Array[String]) {
    val config = new Config(args)

    val generator = generateFromString(config.problemCfg()).fold(msg => sys.error("could not parse problem descriptor:\n" + msg), identity)
    val experiment: Experiment = ???
    val problemReporter: ExperimentReporter[(String,Long,Problem)] = ???
    for{
      problemSeed <- config.problemSeed() until config.problemSeed() + config.problemCount()
      problem = generator(problemSeed)
      _ = printProblem(problem, !config.dontPrintProblem())
      experimentSeed <- config.algorithmSeed() until config.algorithmSeed() + config.algorithmRuns()
    }{
      import ExperimentReporter._
      var run = experiment.create(problem,experimentSeed)
      val reporter: ExperimentReporter[(Int, experiment.State)] =
        problemReporter.freeze((config.problemCfg(),problemSeed,problem)) + experiment.reporter
      println(reporter.header)
      var iteration = 1
      var converged = false
      do {
        val (nextState,converged2) = experiment.step(run)
        converged = converged2
        run = nextState
        println(reporter.experiment((iteration,run)))
        iteration += 1
      } while(!converged)
    }
  }

  trait Experiment{
    type State
    def create(p: Problem, seed: Long): State
    /** @return Next experiment state and true if it shall be continued. */
    def step(a: State): (State,Boolean)
    def reporter: ExperimentReporter[(Int,State)]
  }

  def printProblem(p: Problem, print: Boolean){
    if(print)
      println(p.uaiString)
  }

  /** Provides ln(Z) and variable marginals. */
  def createGroundTruth(p: Problem): (Double, Int => FastFactor) = {
    val jt = new CalibratedJunctionTree(p)
    (jt.logZ,jt.decodedVariableBelief)
  }

  def createAlgorithm(config: String, p: Problem, seed: Long): InfAlg with ConvergingStepper[Unit] =
    new CBP(p,new Random(seed),CBP.leafSelectionSlowestSettler,CBP.variableSelectionSlowestSettler,10,1e-5)
//      new BeliefPropagation(p,new Random(seed))

  class Reporter(graphConf: String, graphSeed: Long, problem: Problem, algConf: String, algSeed: Long, truth: (Double, Int => FastFactor)){
    val startTime = ManagementFactory.getThreadMXBean.getCurrentThreadCpuTime
    def generateHeader {
      println("*>>>>>")
      println("*" +
        Seq(
          "problem.config",
          "problem.seed",
          "gt.lnz",
          "alg.config",
          "alg.seed",
          "est.lnz",
          "mar.kl",
          "mar.maxdiff",
          "mar.meandiff",
          "mar.meansqdiff",
          "iteration",
          "time.cpu")
          .mkString("\t")
      )
    }
    def generateReport(alg: InfAlg, iteration: Int) {
      println(
      "*" +
      Seq[Any](
        graphConf,
        graphSeed,
        truth._1,
        algConf,
        algSeed,
        alg.logZ,
        meanKL(problem,truth._2,alg.decodedVariableBelief),
        maxDiff(problem,truth._2,alg.decodedVariableBelief),
        meanDiff(problem,truth._2,alg.decodedVariableBelief),
        meanSquareDiff(problem,truth._2,alg.decodedVariableBelief),
        iteration,
        getCPUTime
      ).mkString("\t")
      )
    }

    def getCPUTime: Double = (ManagementFactory.getThreadMXBean.getCurrentThreadCpuTime - startTime) * 1e-9
  }

  //evaluation stuff below
  def mapVars[A](problem: Problem, f: Int => A): IndexedSeq[A] = problem.variables.map(f)(collection.breakOut)
  def margStatistics[A,B](problem: Problem, trueMargs: Int => FastFactor, estimate: Int => FastFactor)
                         (map: (FastFactor,FastFactor) => A)
                         (reduce: Seq[A] => B): B = reduce(mapVars(problem,v => map(trueMargs(v),estimate(v))))

  def meanKL(problem: Problem, trueMargs: Int => FastFactor, estimate: Int => FastFactor): Double =
    margStatistics(problem,trueMargs,estimate)(FastFactor.kl)(_.mean)
  def maxDiff(problem: Problem, trueMargs: Int => FastFactor, estimate: Int => FastFactor): Double =
    margStatistics(problem,trueMargs,estimate)(FastFactor.maxDiff)(_.max)
  def meanDiff(problem: Problem, trueMargs: Int => FastFactor, estimate: Int => FastFactor): Double =
    margStatistics(problem,trueMargs,estimate)(FastFactor.maxDiff)(_.mean)
  def meanSquareDiff(problem: Problem, trueMargs: Int => FastFactor, estimate: Int => FastFactor): Double =
    margStatistics(problem,trueMargs,estimate){case (f1,f2) => math.pow(FastFactor.maxDiff(f1,f2),2)}(_.mean)

  def meanDiffReporter(truth: InfAlg) =  ExperimentReporter[InfAlg]("diff.mean",
    ia => meanDiff(truth.getProblem,truth.decodedVariableBelief,ia.decodedVariableBelief).toString)
  def meanKLReporter(truth: InfAlg) =  ExperimentReporter[InfAlg]("kl.mean",
      ia => meanKL(truth.getProblem,truth.decodedVariableBelief,ia.decodedVariableBelief).toString)
  def maxDiffReporter(truth: InfAlg) =  ExperimentReporter[InfAlg]("diff.max",
      ia => maxDiff(truth.getProblem,truth.decodedVariableBelief,ia.decodedVariableBelief).toString)
  def meanSquaredDiffReporter(truth: InfAlg) =  ExperimentReporter[InfAlg]("squarediff.mean",
      ia => meanSquareDiff(truth.getProblem,truth.decodedVariableBelief,ia.decodedVariableBelief).toString)
}

trait ExperimentReporter[-A]{
  def separator: String = "\t"
  def prefix: String = "*"
  def header: String
  def experiment(a: A): String
  def +[B](other: ExperimentReporter[B]) = new ExperimentReporter[(A,B)]{
      def header: String = this.header + separator + other.header
      def experiment(ab: (A,B)): String = ExperimentReporter.this.experiment(ab._1) + separator + other.experiment(ab._2)
    }
  def also[B <: A](other: ExperimentReporter[B]) = new ExperimentReporter[B]{
      def header: String = this.header + separator + other.header
      def experiment(a: B): String = ExperimentReporter.this.experiment(a) + separator + other.experiment(a)
    }
  def comap[B](f: B => A): ExperimentReporter[B] = new ExperimentReporter[B] {
    def experiment(a: B): String = ExperimentReporter.this.experiment(f(a))
    def header: String = ExperimentReporter.this.header
  }
  def freeze(a: A): ExperimentReporter[Unit] = comap(Unit => a)
}

object ExperimentReporter{
  def apply[A](head: String, report: A => String) = new ExperimentReporter[A] {
    def header: String = head
    def experiment(a: A): String = report(a)
  }
  implicit def absorbUnit1[A](r: ExperimentReporter[(Unit,A)]): ExperimentReporter[A] = r.comap((Unit,_))
  implicit def absorbUnit2[A](r: ExperimentReporter[(A,Unit)]): ExperimentReporter[A] = r.comap((_,Unit))
}
