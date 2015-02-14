import sbt._
import sbt.Keys._

object AbBuild extends Build {

  lazy val ab = Project(
    id = "ab",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "ab",
      organization := "ab",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.11.5"
      // add other settings here
    )
  )
}
