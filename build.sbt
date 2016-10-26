name := "abc-bank-scala"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.6" % "test"

resolvers += "johnreed2 bintray" at "http://dl.bintray.com/content/johnreed2/maven"

libraryDependencies += "com.github.johnreedlol" %% "scala-trace-debug" % "3.0.6"