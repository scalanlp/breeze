import sbt.Keys._
import sbt._
import breeze.codegen.plugin.SbtBreezeCodegenPlugin.breezeCodegenSettings
import xerial.sbt.Sonatype.autoImport.{sonatypeProfileName, sonatypeProjectHosting, sonatypePublishTo}
import xerial.sbt.Sonatype._

object Common {

  def priorTo2_13(scalaVersion: String): Boolean = {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor < 13 => true
      case _ => false
    }
  }

  val buildCrossScalaVersions = Seq("2.12.14", "2.13.5")

  lazy val buildScalaVersion = buildCrossScalaVersions.head

  val commonSettings = Seq(
    organization := "org.scalanlp",
    scalaVersion := buildScalaVersion,
    crossScalaVersions := buildCrossScalaVersions,
    scalacOptions ++= Seq("-deprecation", "-language:_"),
    javacOptions ++= Seq("-target", "1.7", "-source", "1.7"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.15.1" % "test",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test",
   //   "org.scala-lang.modules" %% "scala-collection-compat" % "1.0.0"
    ),
    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.typesafeRepo("releases")
    ),
    testOptions in Test += Tests.Argument("-oDF"),
    publishArtifact in Test := false,
    pomIncludeRepository := { _ =>
      false
    },
    libraryDependencies ++= {
      if (priorTo2_13(scalaVersion.value)) {
        Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
      } else {
        Seq.empty
      }
    },
    scalacOptions ++= {
      if (priorTo2_13(scalaVersion.value)) {
        Seq.empty
      } else {
        Seq("-Ymacro-annotations")
      }
    },

    sonatypeProfileName := "org.scalanlp",
    publishMavenStyle := true,
    licenses := Seq("Apache Publich License 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")),
    publishTo := sonatypePublishTo.value,

    // Where is the source code hosted: GitHub or GitLab?
    sonatypeProjectHosting := Some(GitHubHosting("dlwh", "sbt-breeze-expand-codegen", "david.lw.hall@gmail.com"))

  ) ++ breezeCodegenSettings
}
