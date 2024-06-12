import xerial.sbt.Sonatype._

ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name                   := "clowder",
    scalacOptions += "-opt:l:inline",
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-core"        % "2.10.0",
      "org.scalameta" %% "munit-scalacheck" % "1.0.0-M11" % Test
    ),
    publishTo              := sonatypePublishTo.value,
    licenses               := Seq("Apache 2.0" -> url("https://opensource.org/license/apache-2-0")),
    sonatypeProjectHosting := Some(GitHubHosting("andimiller", "clowder", "andi at andimiller dot net")),
    developers             := List(
      Developer(id = "andimiller", name = "Andi Miller", email = "andi@andimiller.net", url = url("http://andimiller.net"))
    )
  )
