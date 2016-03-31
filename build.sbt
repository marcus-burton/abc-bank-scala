name := "abc-bank-scala"

version := "1.0"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
"org.scalatest" % "scalatest_2.11" % "2.1.6" % "test",
"com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
"ch.qos.logback" % "logback-classic" % "1.1.2"
)