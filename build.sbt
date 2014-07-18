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
*/
version := "21.0.0"

homepage := Some(url("http://www.uni-ulm.de/in/ki/staff/thomas-geier.html"))

startYear := Some(2011)

description := "Tools for probabilistic inference in discrete-valued factor graphs with dense factors."

licenses += "MIT" -> url("http://opensource.org/licenses/MIT")

scalaVersion := "2.11.1"

//asserions are only used in tests
scalacOptions in Compile += "-Xdisable-assertions"

libraryDependencies += "de.uni-ulm" %% "vultura-util" % "22.0.0"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"

libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.0.6"
// --------------- Publishing ----------------------------------

//testing dependencies
libraryDependencies += "org.specs2" %% "specs2" % "2.3.11" % "test"

libraryDependencies += "org.scalacheck" % "scalacheck_2.11" % "1.11.3" % "test"

//for vultura-util
resolvers += "tgeier repository@companion" at "http://companion.informatik.uni-ulm.de/~tgeier/mvn"
