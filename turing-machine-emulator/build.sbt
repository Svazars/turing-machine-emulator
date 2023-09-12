ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

mainClass in Compile := Some("nsu.turing.Main")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test

lazy val root = (project in file("."))
  .settings(
    name := "turing-machine-emulator",
    idePackagePrefix := Some("nsu.turing")
  )
