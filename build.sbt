name := "rules_poc"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.7.2" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.7.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)


scalacOptions in Test ++= Seq("-Yrangepos")