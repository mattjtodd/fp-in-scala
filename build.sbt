lazy val commonSettings = Seq(
  organization := "com.mattjtodd",
  version := "0.1.0",
  scalaVersion := "2.12.0"
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(name := "fp-in-scala")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
