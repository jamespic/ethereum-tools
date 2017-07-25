import Dependencies._

scalaVersion := "2.11.11"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "io.github.jamespic",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ethereum-tools",
    libraryDependencies ++= List(
      scalaTest % Test,
      ethereumJ
    )
  )

resolvers += Resolver.bintrayRepo("ethereum", "maven")
