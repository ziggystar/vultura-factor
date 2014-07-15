name := "vultura-util"

organization := "de.uni-ulm"

/*
17:     add parameter learning
18:     refactoring, remove some stuff
19:     extract statistics stuff into vultura.util.stats._
20.0.0: add new ConditionedInference implementation and new faster LBP
20.0.1: a little work on CI inference stuff
20.0.2: fix soft-factor normalization and some refactoring
20.0.3: tie-breaking for variable selection heuristics in ConditionedInference
*/
version := "20.0.3"

homepage := Some(url("http://www.uni-ulm.de/in/ki/staff/thomas-geier.html"))

startYear := Some(2011)

description := "Tools for probabilistic inference in discrete-valued factor graphs with dense factors."

licenses += "MIT" -> url("http://opensource.org/licenses/MIT")

scalaVersion := "2.11.1"

//asserions are only used in tests
scalacOptions in Compile += "-Xdisable-assertions"

//scalaz
libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.0.6"

// --------------- Java libraries ------------------------------
libraryDependencies += "net.sf.trove4j" % "trove4j" % "3.0.3"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"
// --------------- Publishing ----------------------------------

//testing dependencies
libraryDependencies += "org.specs2" %% "specs2" % "2.3.11" % "test"

libraryDependencies += "org.scalacheck" % "scalacheck_2.11" % "1.11.3" % "test"
