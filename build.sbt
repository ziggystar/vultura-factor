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
20.0.4: moved code from competition solver into library; added code for OpenBitSet from apache lucene
21.0.0: move probabilistic inference code into separate project
22.0.0: remove scalaz dependency
23.0.0: Index now takes Iterables thus one can provide a target ordre of the indices.
23.1.0: - add simple tree test to TreeWidth
23.1.1: - upgrade some dependencies; add Memo
23.2.0:
  - tiny optimization for construction of junction trees
  - add functionality to compute a DAG-Cover to EdgeMapDiGraph
24.0.0:
  - add Dist probability monad
  - add new graphviz framework that supports directed and undirected graphs (previously only directed graphs were supported)
  - add new graph API
24.0.1:
  - fix bug in tree decomposition, heuristic was completely broken and crashed on trees
24.0.2:
  - fix Java target version to 1.6
*/
version := "24.0.2"

homepage := Some(url("http://www.uni-ulm.de/in/ki/staff/thomas-geier.html"))

startYear := Some(2011)

description := "Tools for probabilistic inference in discrete-valued factor graphs with dense factors."

licenses += "MIT" -> url("http://opensource.org/licenses/MIT")

scalaVersion := "2.11.7"

javacOptions ++= Seq("-source", "1.6", "-target", "1.6")

//asserions are only used in tests
scalacOptions in Compile += "-Xdisable-assertions"

// --------------- Java libraries ------------------------------
libraryDependencies += "net.sf.trove4j" % "trove4j" % "3.0.+"
// --------------- Publishing ----------------------------------

//testing dependencies
libraryDependencies += "org.specs2" %% "specs2-core" % "3.6.4" % "test"
