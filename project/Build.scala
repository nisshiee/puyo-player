import sbt._
import Keys._

object PuyoPlayerRootBuild extends Build {
  lazy val player = Project(
    id = "player",
    base = file("player"))

  lazy val runner = Project(
    id = "runner",
    base = file("runner")) dependsOn player
}
