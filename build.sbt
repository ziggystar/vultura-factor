name := "vultura-util"

organization := "de.uni-ulm"

/*
17: add parameter learning
18: refactoring, remove some stuff
*/
version := "18-SNAPSHOT"

homepage := Some(url("http://www.uni-ulm.de/in/ki/staff/thomas-geier.html"))

startYear := Some(2011)

description := "Tools for probabilistic inference in discrete-valued factor graphs with dense factors."

licenses += "MIT" -> url("http://opensource.org/licenses/MIT")

scalaVersion := "2.11.0"

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

publishMavenStyle := true

publishTo <<= version { version: String =>
  val repoInfo = "tgeier releases" -> "/media/SFB-Space/SambaLDAP/HOMES/tgeier/public_html/mvn"
  val user = System.getProperty("user.name")
  val keyFile = (Path.userHome / ".ssh" / "id_rsa").asFile
  Some(Resolver.ssh(
    repoInfo._1,
    "companion.informatik.uni-ulm.de",
    repoInfo._2) as(user, keyFile) withPermissions("0644"))
}
