name := "rules_poc"

version := "1.0"

scalaVersion := "2.11.8"

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.7.1" cross CrossVersion.binary)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.7" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.7" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test",
  "org.typelevel" %% "cats" % "0.4.1" % "test"
)


scalacOptions in Test ++= Seq("-Yrangepos")
