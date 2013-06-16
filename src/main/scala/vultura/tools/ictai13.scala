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
import vultura.experiments.{Reporter, Experiment}

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

    val experiment: Experiment[(InfAlg,InfAlg)] = for{
      _ <- Experiment.description("config.problem")(config.problemCfg())
        .withReport(Reporter.constant("config.algorithm",config.algorithmCfg()))
      pi <- Experiment.generateSeed("seed.problem")(config.problemSeed(),config.problemCount())
      problem = generator(pi)
      groundTruth = createGroundTruth(problem)
      algorithmSeed <- Experiment.generateSeed("seed.algorithm")(config.algorithmSeed(),config.algorithmRuns())
      algorithm = createAlgorithm(config.problemCfg(),problem,algorithmSeed)
    } yield (groundTruth,algorithm)

    val finalExperiment = experiment
      .withReport(meanDiffReporter)
      .withReport(meanKLReporter)
      .withReport(meanSquaredDiffReporter)
      .withReport(maxDiffReporter)

    finalExperiment.run(System.out)
  }

  def printProblem(p: Problem, print: Boolean){
    if(print)
      println(p.uaiString)
  }

  /** Provides ln(Z) and variable marginals. */
  def createGroundTruth(p: Problem): InfAlg = new CalibratedJunctionTree(p)

  def createAlgorithm(config: String, p: Problem, seed: Long): InfAlg with ConvergingStepper[Unit] =
    new CBP(p,new Random(seed),CBP.leafSelectionSlowestSettler,CBP.variableSelectionSlowestSettler,10,1e-5)
//      new BeliefPropagation(p,new Random(seed))

//    def getCPUTime: Double = (ManagementFactory.getThreadMXBean.getCurrentThreadCpuTime - startTime) * 1e-9

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

  def meanDiffReporter: Reporter[(InfAlg,InfAlg)] =  Reporter[(InfAlg,InfAlg)]("diff.mean")(
    ia => meanDiff(ia._1.getProblem,ia._1.decodedVariableBelief,ia._2.decodedVariableBelief).toString)
  def meanKLReporter: Reporter[(InfAlg,InfAlg)] =  Reporter[(InfAlg,InfAlg)]("kl.mean")(
      ia => meanKL(ia._1.getProblem,ia._1.decodedVariableBelief,ia._2.decodedVariableBelief).toString)
  def maxDiffReporter: Reporter[(InfAlg,InfAlg)] =  Reporter[(InfAlg,InfAlg)]("diff.max")(
      ia => maxDiff(ia._1.getProblem,ia._1.decodedVariableBelief,ia._2.decodedVariableBelief).toString)
  def meanSquaredDiffReporter: Reporter[(InfAlg,InfAlg)] =  Reporter[(InfAlg,InfAlg)]("squarediff.mean")(
      ia => meanSquareDiff(ia._1.getProblem,ia._1.decodedVariableBelief,ia._2.decodedVariableBelief).toString)
}
