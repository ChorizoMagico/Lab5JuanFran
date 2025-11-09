ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "Lab5"
  )

libraryDependencies += "org. plotly-scala" %%"plotly-render" % "0.8.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
