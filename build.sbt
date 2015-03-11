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
*/
version := "22.3.0"

homepage := Some(url("http://www.uni-ulm.de/in/ki/staff/thomas-geier.html"))

startYear := Some(2011)

description := "Tools for probabilistic inference in discrete-valued factor graphs with dense factors."

licenses += "MIT" -> url("http://opensource.org/licenses/MIT")

scalaVersion := "2.11.6"

//assertions are only used in tests
scalacOptions in Compile += "-Xdisable-assertions"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"

libraryDependencies += "de.uni-ulm" %% "vultura-util" % "23.1.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.+"


// --------------- Publishing ----------------------------------

//testing dependencies
libraryDependencies += "org.specs2" %% "specs2" % "2.3.13" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"

//for vultura-util
resolvers += "mvn@mirkwood" at "http://mirkwood.informatik.uni-ulm.de/mvn"

//--- fixing exit code for jenkins
testResultLogger in (Test, test) := new TestResultLogger {
  import sbt.Tests._
  def run(log: Logger, results: Output, taskName: String): Unit = {
    println("Exit code always 0...as you wish")
    // uncomment to have the default behaviour back
    // TestResultLogger.SilentWhenNoTests.run(log, results, taskName)
  }
}