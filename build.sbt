ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
	.settings(
		name := "euler100-scala"
	)

libraryDependencies ++=
	Seq(
		"org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
		"org.scalatest" %% "scalatest" % "3.2.15" % "test",
		"org.scalaz" %% "scalaz-core" % "7.3.7"
	)

Test / parallelExecution := true