import xerial.sbt.Sonatype._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

inThisBuild(Seq(
  organization := "com.olegpy",
  scalaVersion := "2.12.8",
  version := "0.1.0-SNAPSHOT",
  crossScalaVersions := Seq("2.12.8"),
  pgpPassphrase := sys.env.get("PGP_PASS").map(_.toArray),
))

lazy val root = project.in(file("."))
  .aggregate(stm4cats.js, stm4cats.jvm)
  .settings(
    name := "stm4cats",
    publish := {},
    publishLocal := {},
    publishArtifact := false,
  )

lazy val stm4cats = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
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

    credentials += Credentials(
      "Sonatype Nexus Repository Manager",
      "oss.sonatype.org",
      sys.env.getOrElse("SONATYPE_USER", ""),
      sys.env.getOrElse("SONATYPE_PASS", "")
    ),
  )
