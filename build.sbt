ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name := "clowder",
    scalacOptions += "-opt:l:inline",
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-core"        % "2.10.0",
      "org.scalameta" %% "munit-scalacheck" % "1.0.0-M11" % Test
    )
  )
