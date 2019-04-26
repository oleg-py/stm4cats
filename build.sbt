import xerial.sbt.Sonatype._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

inThisBuild(Seq(
  organization := "com.olegpy",
  scalaVersion := "2.12.8",
  version := "0.1.0-SNAPSHOT",
  crossScalaVersions := Seq("2.12.8"),
))

lazy val stm4cats = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .withoutSuffixFor(JVMPlatform)
  .in(file("."))
  .settings(
    name := "stm4cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % "1.2.0",
      "com.lihaoyi" %%% "utest" % "0.6.7" % Test,
    ),

    testFrameworks += new TestFramework("utest.runner.Framework"),

    scalacOptions --= Seq(
      "-Xfatal-warnings",
      "-Ywarn-unused:params",
      "-Ywarn-unused:implicits",
    ),
    publishTo := sonatypePublishTo.value,
    publishMavenStyle := true,
    sonatypeProjectHosting :=
      Some(GitHubHosting("oleg-py", "stm4cats", "oleg.pyzhcov@gmail.com")),
  )
