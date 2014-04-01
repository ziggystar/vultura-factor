resourceGenerators in Compile <+= Def.task {
  import scala.sys.process._
  val file = (resourceManaged in Compile).value / "vultura" / "version.properties"
  val gitHash = "git rev-parse HEAD".!!
  val gitStatus = "git status".!!
  IO.writeLines(file, Seq(
    s"project.version = ${version.value}",
    s"git.hash = $gitHash",
    s"build.date = ${new java.util.Date()}") ++
    gitStatus.split("\n").map("#" + _))
  Seq(file)
}