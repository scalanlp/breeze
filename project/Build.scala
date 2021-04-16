import sbt.Keys._
import sbt._
import breeze.codegen.plugin.SbtBreezeCodegenPlugin.breezeCodegenSettings
import xerial.sbt.Sonatype.autoImport.{sonatypeProfileName, sonatypeProjectHosting, sonatypePublishTo}
import xerial.sbt.Sonatype._

import dotty.tools.sbtplugin.DottyPlugin.autoImport._

object Common {

  def priorTo2_13(scalaVersion: String): Boolean = {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor < 13 => true
      case _ => false
    }
  }

  val buildCrossScalaVersions = Seq("3.0.0-RC2", "2.12.10", "2.13.3")

  lazy val buildScalaVersion = buildCrossScalaVersions.head

  val commonSettings = Seq(
    organization := "org.scalanlp",
    scalaVersion := buildScalaVersion,
    crossScalaVersions := buildCrossScalaVersions,
    scalacOptions ++= Seq("-deprecation", "-language:_"),
    javacOptions ++= Seq("-target", "1.7", "-source", "1.7"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),

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
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch),
          "org.scalatest" %% "scalatest" % "3.2.7" % "test",
          ("org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1" % "test"),
          ("org.scalacheck" %% "scalacheck" % "1.14.3" % "test")
        ),
      } else {
        ("org.scalatest" %% "scalatest" % "3.2.7" % "test") +:
        Seq(
          
          ("org.scalatestplus" % "scalacheck-1-14_2.13" % "3.1.1.1" % "test")
            .intransitive(),
          "org.scalacheck" % "scalacheck_2.13" % "1.14.3" % "test"
        ).map(_.withDottyCompat(scalaVersion.value))
      }
    },
    scalacOptions ++= {
      if (priorTo2_13(scalaVersion.value)) {
        Seq.empty
      } else {
        Seq("-Ymacro-annotations", "-language:implicitConversions")
      }
    },

    sonatypeProfileName := "org.scalanlp",
    publishMavenStyle := true,
    licenses := Seq("Apache Public License 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")),
    publishTo := sonatypePublishTo.value,

    // Where is the source code hosted: GitHub or GitLab?
    sonatypeProjectHosting := Some(GitHubHosting("dlwh", "sbt-breeze-expand-codegen", "david.lw.hall@gmail.com")),

    unmanagedSourceDirectories in Compile ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11|12)) => Seq(
          baseDirectory.value / "src" / "main" / "scala_2.11_2.12",
          baseDirectory.value / "src" / "main" / "scala_2",
        )
        case Some((2, 13)) => Seq(
          baseDirectory.value / "src" / "main" / "scala_2",
          baseDirectory.value / "src" / "main" / "scala_2.13"
        )
        case Some( (3, _)) => Seq(
          baseDirectory.value / "src" / "main" / "scala_2.13",
          baseDirectory.value / "src" / "main" / "scala_3"
        )
        case _ => ???
      }
    }
  ) ++ breezeCodegenSettings
}
