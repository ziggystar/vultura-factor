name := "vultura-factor"

organization := "de.uni-ulm"

/*
17:     add parameter learning
18:     refactoring, remove some stuff
19:     extract statistics stuff into vultura.util.stats._
20.0.0: add new ConditionedInference implementation and new faster LBP
20.0.1: a little work on CI inference stuff
20.0.2: fix soft-factor normalization and some refactoring
20.0.3: tie-breaking for variable selection heuristics in ConditionedInference
20.0.4: moved code from competition solver into library; added code for OpenBitSet from apache lucene
21.0.0: extract probabilistic inference code into separate project
22.0.0: fix different LCBP implementations
22.1.0: Add fully factorized CBP inference; some refactorings
22.2.0: add interactions on second layer of two-tier grid problem generator
22.3.0: fix merge/commit accident where FullyConditionedBP from 22.1.0 was missing from 22.2.0
22.3.1: use a non-leaking result class for LBP.inferWithStats
22.4.0: add support for condition-weight correction term to some LCBP implementations
22.4.1: make GridProblem available outside test code
22.5.0:
  - tikz output for FactoredScheme
  - optimization for parameter learning
  - vultura.factor.inference.BeliefPropagation now supports damping
22.5.1: fix to uai-string
22.5.2: fix bug in parameter learning introduced in commit 6f8550e742114ab6aed9e07da0770d011d120b51
23.0.0: add new problem generator library; see readme
23.0.2-DEV: add nicer generator code
*/
version := "23.0.2-DEV"

homepage := Some(url("http://www.uni-ulm.de/in/ki/staff/thomas-geier.html"))

startYear := Some(2011)

description := "Tools for probabilistic inference in discrete-valued factor graphs with dense factors."

licenses += "MIT" -> url("http://opensource.org/licenses/MIT")

scalaVersion := "2.11.7"

//assertions are only used in tests
scalacOptions in Compile += "-Xdisable-assertions"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"

libraryDependencies += "de.uni-ulm" %% "vultura-util" % "23.2.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.+"


// --------------- Publishing ----------------------------------

//testing dependencies
libraryDependencies += "org.specs2" %% "specs2-core" % "3.6.4-20150901013911-1f41c5e" % "test"

libraryDependencies += "org.specs2" % "specs2-scalacheck_2.11" % "3.6.4-20150901013911-1f41c5e" % "test"


scalacOptions in Test ++= Seq("-Yrangepos") //for specs2

//for vultura-util
resolvers += "mvn@mirkwood" at "http://mirkwood.informatik.uni-ulm.de/mvn"

//--- fixing exit code for jenkins
testResultLogger in (Test, testOnly) := new TestResultLogger {
  import sbt.Tests._
  def run(log: Logger, results: Output, taskName: String): Unit = {
    println("Exit code always 0...as you wish")
    // uncomment to have the default behaviour back
    // TestResultLogger.SilentWhenNoTests.run(log, results, taskName)
  }
}
