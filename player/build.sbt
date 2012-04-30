name := "puyo-player"

organization := "org.nisshiee"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.9.1"

resolvers += "github nisshiee puyo" at "http://nisshiee.github.com/puyo-scala-wrapper/repo/release/"

libraryDependencies := Seq(
   "org.scalaz" %% "scalaz-core" % "6.0.4"
  ,"org.nisshiee" %% "puyo-scala-wrapper" % "1.0.1"
  ,"com.github.scala-incubator.io" %% "scala-io-core" % "0.3.0"
  ,"com.github.scala-incubator.io" %% "scala-io-file" % "0.3.0"
  ,"org.specs2" %% "specs2" % "1.9" % "test"
  ,"org.mockito" % "mockito-all" % "1.9.0" % "test"
  ,"junit" % "junit" % "4.10" % "test"
  ,"org.pegdown" % "pegdown" % "1.1.0" % "test"
)

testOptions in (Test, test) += Tests.Argument("console", "html", "junitxml")

initialCommands := """
import scalaz._
import Scalaz._
import scalax.io._
import scalax.file._
import Path._
"""

seq(assemblySettings: _*)

dependencyClasspath in Runtime ~= { d => d filter(!_.data.getName.startsWith("PuyopuyoVer")) }
