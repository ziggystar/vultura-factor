package vultura.tools

import org.rogach.scallop.{ValueConverter, ScallopConf}
import java.io._
import org.rogach.scallop
import vultura.fastfactors._
import generators._
import vultura.fastfactors.algorithms._
import scala.util.Random
import vultura.fastfactors.Problem
import vultura.util._
import vultura.experiments.{Exp, Reporter}
import scala.util.parsing.combinator.JavaTokenParsers
import scala.Enumeration
import scala.Some
import vultura.fastfactors.algorithms.CBPConfig

import language.reflectiveCalls

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
    val chunkSize = opt[Int](
      name = "chunk-size",
      descr = "max number of parallel algorithm evaluations",
      short = 'P',
      default = Some(4)
    )
  }

  /*
    Each output line contains:
    <problem-cfg> <problem-seed> <PR> <algorithm-cfg> <algorithm-seed> <PR estimate> <MAR KL> <MAR maxdiff> <MAR mean maxDiff> <iteration> <CPU time>
     */
  def main(args: Array[String]) {
    val config = new Config(args)

    val generator: Exp[Long => Problem] =
      ProblemSourceParser.parse(config.problemCfg()).fold(msg => sys.error("could not parse problem descriptor:\n" + msg), identity)

    val chunkSize = config.chunkSize()

    val experiment: Exp[(Problem, AlgConfig, Long, InfAlg)] = for {
      _ <- Exp()
        .addColumn("config.problem", _ => config.problemCfg())
        .addColumn("config.algorithm",_ => config.algorithmCfg())
      pi <- Exp.seed(config.problemSeed(),config.problemCount(),"seed.problem")
      problemGenerator <- generator
      problem = problemGenerator(pi)
      _ <- Exp.values(problem)
        .withReport(numVars())
        .withReport(numFactors())
        .withReport(maxDomainSize())
      groundTruth <- Exp.values(createGroundTruth(problem))
        .withReport(logZReporter("true.lnZ"))
      algorithmSeed <- Exp.seed(config.algorithmSeed(), config.algorithmRuns(),"seed.algorithm")
      conf: AlgConfig <- AlgConfParser.parse(config.algorithmCfg())
    } yield (problem,conf,algorithmSeed,groundTruth)

    val parExp = for{
      (problem,conf,algorithmSeed,groundTruth) <- experiment.parallel(chunkSize)
      _ <- Exp.fromIterator(conf.iterator(problem,algorithmSeed).take(config.algorithmSteps()))
        .withReport(logZReporter("estimate.lnZ"))
        .withReport(meanDiffReporter(groundTruth))
        .withReport(meanKLReporter(groundTruth))
        .withReport(meanSquaredDiffReporter(groundTruth))
        .withReport(maxDiffReporter(groundTruth))
        .withReport(iaIteration("iteration.alg"))
    } yield Unit: Unit

    val result = parExp.create
    println(result._1.mkString("\t"))
    result._2.foreach(row => println(row.mkString("\t")))
  }

  def printProblem(p: Problem, print: Boolean){
    if(print)
      println(p.uaiString)
  }

  /** Provides ln(Z) and variable marginals. */
  def createGroundTruth(p: Problem): InfAlg = new CalibratedJunctionTree(p).toResult

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

  def meanDiffReporter(gt: InfAlg): Reporter[InfAlg] =  Reporter[InfAlg]("diff.mean")(
    ia => meanDiff(gt.getProblem,gt.decodedVariableBelief,ia.decodedVariableBelief).toString)
  def meanKLReporter(gt: InfAlg): Reporter[InfAlg] =  Reporter[InfAlg]("kl.mean")(
      ia => meanKL(gt.getProblem,gt.decodedVariableBelief,ia.decodedVariableBelief).toString)
  def maxDiffReporter(gt: InfAlg): Reporter[InfAlg] =  Reporter[InfAlg]("diff.max")(
      ia => maxDiff(gt.getProblem,gt.decodedVariableBelief,ia.decodedVariableBelief).toString)
  def meanSquaredDiffReporter(gt: InfAlg): Reporter[InfAlg] =  Reporter[InfAlg]("squarediff.mean")(
      ia => meanSquareDiff(gt.getProblem,gt.decodedVariableBelief,ia.decodedVariableBelief).toString)

  def iaIteration(name: String = "iteration"): Reporter[InfAlg] = Reporter(name)(_.iteration.toString)
  def logZReporter(name: String = "lnZ"): Reporter[InfAlg] = Reporter(name)(_.logZ.toString)

  def numFactors(name: String = "problem.factors"): Reporter[Problem] = Reporter(name)(_.factors.size.toString)
  def numVars(name: String = "problem.variables"): Reporter[Problem] = Reporter(name)(_.variables.size.toString)
  def maxDomainSize(name: String = "problem.domainsize.max"): Reporter[Problem] = Reporter(name)(_.domains.max.toString)
}

object ProblemSourceParser extends JavaTokenParsers {
  def int: Parser[Int] = wholeNumber ^^ (_.toInt)
  def float: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def expAll[A](p: Parser[A]): Parser[Exp[A]] = "{" ~> repsep(p,",") <~ "}" ^^ (as => Exp.values(as:_*)) | p ^^ (Exp.values(_))

  def expInt: Parser[Exp[Int]] = expAll(int)
  def expFloat: Parser[Exp[Double]] = expAll(float)

  def expPots: Parser[Exp[FactorGenerator]] = pFmaxEnt | pFexpGauss | pFdetClause | pFSigmaClause
  def pFmaxEnt: Parser[Exp[FactorGenerator]] = "max-entropy" ^^^ Exp.values(maxEntropy)
  def pFdetClause: Parser[Exp[FactorGenerator]] = "det-clause" ^^^ Exp.values(deterministicClause)
  def pFexpGauss: Parser[Exp[FactorGenerator]] =
    for(eSigma <- "expgauss(" ~> expFloat <~ ")") yield for(sigma <- eSigma) yield expGauss(sigma)
  def pFSigmaClause: Parser[Exp[FactorGenerator]] =
    for( eSigma <- "clause(" ~> expFloat <~ ")") yield for(sigma <- eSigma) yield sigmaClause(sigma)

  def grid: Parser[Exp[Long => Problem]] =
    for{
      ex ~ ey ~ edoms ~ epots <- "grid(" ~> (expInt <~ ",") ~ (expInt <~ ",") ~ (expInt <~ ",") ~ expPots <~ ")"
    } yield for{
      x <- ex
      y <- ey
      doms <- edoms
      pots <- epots
    } yield (seed: Long) => generators.grid(x,y,doms,pots,new Random(seed))

  def randomK: Parser[Exp[Long => Problem]] =
    for{
      eV ~ eE ~ eK ~ eDoms ~ epots <- "randomK(" ~> (expInt <~ ",") ~ (expInt <~ ",") ~ (expInt <~ ",") ~ (expInt <~ ",") ~ expPots <~ ")"
    } yield for{
      v <- eV
      e <- eE
      k <- eK
      doms <- eDoms
      pots <- epots
    } yield (seed: Long) => generators.randomK(v,e,k,doms,pots,new Random(seed))

  def pGen: Parser[Exp[(Long) => Problem]] = grid | randomK

  def parse(s: String): Either[String,Exp[Long => Problem]] = {
    parseAll(pGen,s) match {
      case Success(alg,_) => Right(alg)
      case ns@NoSuccess(msg,_) => Left(sys.error(msg + "\n" + ns))
    }
  }
}

object AlgConfParser extends JavaTokenParsers {
  val cbpReporter: Reporter[CBPConfig] = Reporter(
    Seq("bp.tol","bp.maxiter","cbp.sel.leaf","cbp.sel.var","cbp.clamp")
  )(
    cfg => Seq(cfg.bpTol.toString,cfg.bpMaxiter.toString,cfg.leafSelection.toString,cfg.variableSelection.toString,cfg.clampMethod.toString)
  )

  def algConf: Parser[Exp[AlgConfig]] = "CBP[" ~> cbpModE <~ "]" ^^ {
    case cbpModExp => for {
      conf <- Exp.values(cbpModExp:_*)
      mod = conf(CBPConfig())
      _ <- Exp.values(mod).withReport(cbpReporter)
    } yield mod: AlgConfig
  }

  def cbpModE: Parser[List[CBPConfig => CBPConfig]] = repsep(leafSelE | varSelE | clampMethodE | tolE | bpIterE, ",") ^^ {
    case expMods => expMods.foldLeft(List(identity[CBPConfig] _))((mod1,mod2) => for(f1 <- mod1;f2 <- mod2) yield f1 compose f2)
  }

  def leafSelE: Parser[List[CBPConfig => CBPConfig]] =
    parseList("leafsel",leafSelString) ^^ (_.map(method => (_: CBPConfig).copy(leafSelection=method)))

  def varSelE: Parser[List[CBPConfig => CBPConfig]] =
    parseList("varsel",varSelString) ^^ (_.map(method => (_: CBPConfig).copy(variableSelection=method)))

  def clampMethodE: Parser[List[CBPConfig => CBPConfig]] =
    parseList[CBP.CLAMP_METHOD.Value]("clamp", clampMethodString) ^^ (_.map(method => (_: CBPConfig).copy(clampMethod=method)))

  def tolE: Parser[List[CBPConfig => CBPConfig]] =
    parseList[Double]("tol", floatingPointNumber ^^ (_.toDouble)) ^^ (_.map(x => (_: CBPConfig).copy(bpTol=x)))

  def bpIterE: Parser[List[CBPConfig => CBPConfig]] =
    parseList[Int]("bpiter", wholeNumber ^^ (_.toInt)) ^^ (_.map(x => (_: CBPConfig).copy(bpMaxiter=x)))

  def parseList[A](name: String, parser: Parser[A]): Parser[List[A]] =
    name ~ "=" ~> ("{" ~> rep1sep(parser,",")<~ "}" | parser ^^ (x => List(x)))

  def varSelString: Parser[CBP.VARIABLE_SELECTION.Value] = enumParser(CBP.VARIABLE_SELECTION)
  def clampMethodString: Parser[CBP.CLAMP_METHOD.Value] = enumParser(CBP.CLAMP_METHOD)
  def leafSelString: Parser[CBP.LEAF_SELECTION.Value] = enumParser(CBP.LEAF_SELECTION)

  def enumParser[A](enum: Enumeration): Parser[enum.Value] =
    enum.values.map(value => value.toString ^^^ value).reduce(_ | _)

  def parse(s: String): Exp[AlgConfig] = {
    parseAll(algConf,s) match {
      case Success(alg,_) => alg
      case ns@NoSuccess(msg,_) => sys.error(msg + "\n" + ns)
    }
  }
}

