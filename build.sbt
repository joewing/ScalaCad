import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

organization := "net.joewing"
name := "scalacad"

homepage := Some(url("https://github.com/joewing/ScalaCad"))
startYear := Some(2018)
description := "A Scala DSL for Constructive Solid Geometry"
licenses += ("BSD Simplified", url("https://opensource.org/licenses/BSD-3-Clause"))

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.scalatest"          %% "scalatest"                % "3.0.0" % "test"
)

releaseIgnoreUntrackedFiles := true
releaseProcess := Seq[ReleaseStep](
   inquireVersions,
   runTest,
   setReleaseVersion,
   commitReleaseVersion,
   tagRelease,
   publishArtifacts,
   setNextVersion,
   commitNextVersion,
   pushChanges
)

bintrayOrganization := Some("joewing")
bintrayRepository := "maven"
bintrayPackageLabels := Seq("scala", "csg", "cad")
