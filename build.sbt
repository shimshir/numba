ThisBuild / scalaVersion := "2.13.2"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "shimshir"
ThisBuild / organizationName := "shimshir"

lazy val root = (project in file("."))
  .settings(
    name := "numba",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.1.1" % Test
    )
  )

scalacOptions ++= Seq("-deprecation", "-feature")
