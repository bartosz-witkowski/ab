import sbt._
import sbt.Keys._

object AbBuild extends Build {
  // from http://tpolecat.github.io/2014/04/11/scalac-flags.html
  val pedanticScalac = Seq(
    "-deprecation",           
    "-encoding", "UTF-8",       
    "-feature",                
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",       
    "-Xlint",
    "-Yno-adapted-args",       
    "-Ywarn-dead-code",        
    "-Ywarn-numeric-widen",   
    "-Ywarn-value-discard",
    "-Xfuture",
    "-Ywarn-unused-import"     
  )

  lazy val ab = Project(
    id = "ab",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "ab",
      organization := "ab",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.11.5",
      scalacOptions ++= pedanticScalac
      // add other settings here
    )
  )
}
