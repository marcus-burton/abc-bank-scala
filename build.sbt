name := "abc-bank-scala"

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test

scalafmtOnCompile := true

fork in Test := true

// Exclude slow tests by default.  Use `slow:test` to run (only) them.
testOptions in Test += Tests.Argument("-l", "org.scalatest.tags.Slow")
lazy val Slow = config("slow").extend(Test)
configs(Slow)
inConfig(Slow)(Defaults.testTasks)
testOptions in Slow -= Tests.Argument("-l", "org.scalatest.tags.Slow")
testOptions in Slow += Tests.Argument("-n", "org.scalatest.tags.Slow")
