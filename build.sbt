ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.8"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaLab"
  )

//LibraryDependencies ++= Seq(
//  "com.lihaoyi" %% "os-lib" % "0.11.3",
//  "com.lihaoyi" %% "upickle" % "3.3.1"
//)

libraryDependencies += "org.scala-lang" %% "toolkit" % "0.7.0"
