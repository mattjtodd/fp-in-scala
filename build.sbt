lazy val commonSettings = Seq(
  organization := "com.mattjtodd",
  version := "0.1.0",
  scalaVersion := "2.12.0"
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(name := "fp-in-scala")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
