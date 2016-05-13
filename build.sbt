name := "abc-bank-scala"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies  ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.4",
  "joda-time" % "joda-time" % "2.9.3",
  "org.joda" % "joda-convert" % "1.2",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)