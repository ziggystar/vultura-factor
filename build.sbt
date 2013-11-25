name := "vultura.util"

organization := "de.uni-ulm"

/*
14.1:
 - add method to CBP that gives the entropy over the condition
 */
version := "14.1"

scalaVersion := "2.10.2"

//specs2 dependency
libraryDependencies += "org.specs2" % "specs2_2.10" % "2.3.4" % "test"

libraryDependencies += "org.scalacheck" % "scalacheck_2.10" % "1.11.0"

//scalaz
libraryDependencies += "org.scalaz" % "scalaz-core_2.10" % "7.0.2"


//cli parsing
libraryDependencies += "org.rogach" % "scallop_2.10" % "0.9.2"

//logging
libraryDependencies += "com.dongxiguo" % "zero-log_2.10" % "0.3.3"

// --------------- Java libraries ------------------------------
libraryDependencies += "net.sf.trove4j" % "trove4j" % "3.0.3"

// --------------- Publishing ----------------------------------

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
