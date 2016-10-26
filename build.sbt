name := "abc-bank-scala"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.6" % "test"

resolvers += "johnreed2 bintray" at "http://dl.bintray.com/content/johnreed2/maven"

libraryDependencies += "com.github.johnreedlol" %% "scala-trace-debug" % "3.0.6"

scalacOptions ++= Seq("-Xfatal-warnings", "-unchecked", "-feature", "-Xlint", "-Yinline-warnings", "-Ywarn-inaccessible", "-Ywarn-nullary-override", "-Ywarn-nullary-unit")

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.14.0"
