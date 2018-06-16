name := "scalacad"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.scalanlp"           %% "breeze"                   % "0.13.1",
  "org.scalatest"          %% "scalatest"                % "3.0.0" % "test"
)
