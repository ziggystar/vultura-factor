name := "vultura.util"

organization := "de.uni-ulm"

version := "12.6-SNAPSHOT"

scalaVersion := "2.9.2"

//specs2 dependency
libraryDependencies += "org.specs2" %% "specs2" % "1.12.1" % "test"

//scalaz
libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.3"

publishMavenStyle := true

publishTo <<= (version) { version: String =>
  val repoInfo = ( "tgeier releases" -> "/media/SFB-Space/SambaLDAP/HOMES/tgeier/public_html/mvn" )
  val user = System.getProperty("user.name")
  val keyFile = (Path.userHome / ".ssh" / "id_rsa").asFile
  Some(Resolver.ssh(
    repoInfo._1,
    "companion.informatik.uni-ulm.de",
    repoInfo._2) as(user, keyFile) withPermissions("0644"))
}
