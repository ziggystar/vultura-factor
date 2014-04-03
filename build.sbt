name := "vultura.util"

organization := "de.uni-ulm"

/*
17: add parameter learning
18: refactoring, remove some stuff
*/
version := "18-SNAPSHOT"

scalaVersion := "2.10.4"

scalacOptions in Compile += "-Xdisable-assertions"

//scalaz
libraryDependencies += "org.scalaz" % "scalaz-core_2.10" % "7.0.3"

//scala-arm
libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3"

// --------------- Java libraries ------------------------------
libraryDependencies += "net.sf.trove4j" % "trove4j" % "3.0.3"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"
// --------------- Publishing ----------------------------------

//testing dependencies
libraryDependencies += "org.specs2" % "specs2_2.10" % "2.3.4" % "test"

libraryDependencies += "org.scalacheck" % "scalacheck_2.10" % "1.11.0" % "test"

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
