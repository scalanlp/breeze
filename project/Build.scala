import sbt.Keys._
import sbt._

import dotty.tools.sbtplugin.DottyPlugin.autoImport._

object Common {

  def priorTo2_13(scalaVersion: String): Boolean = {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor < 13 => true
      case _ => false
    }
  }

  val buildCrossScalaVersions = Seq("2.12.10", "2.13.3", "3.0.0-M3")

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
    pomExtra :=
      <url>http://scalanlp.org/</url>
        <licenses>
          <license>
            <name>Apache 2</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <developers>
          <developer>
            <id>dlwh</id>
            <name>David Hall</name>
            <url>http://www.dlwh.org/</url>
          </developer>
        </developers>,
    publishMavenStyle := true,
    publishTo := {
      val yes = isSnapshot.value
      val nexus = "https://oss.sonatype.org/"
      if (yes)
        Some("snapshots".at(nexus + "content/repositories/snapshots"))
      else
        Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
    },
    publishArtifact in Test := false,
    pomIncludeRepository := { _ =>
      false
    },
    libraryDependencies ++= {
      if (priorTo2_13(scalaVersion.value)) {
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch),
          "org.scalatest" %% "scalatest" % "3.1.1" % "test",
          ("org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1" % "test"),
          ("org.scalacheck" %% "scalacheck" % "1.14.3" % "test")
        ),
      } else {
        Seq(
          "org.scalatest" %% "scalatest" % "3.1.1" % "test",
          ("org.scalatestplus" % "scalacheck-1-14_2.13" % "3.1.1.1" % "test")
            .intransitive()
            .withDottyCompat(scalaVersion.value),
          ("org.scalacheck" % "scalacheck_2.13" % "1.14.3" % "test").withDottyCompat(scalaVersion.value)
        )
      }
    },
    scalacOptions ++= {
      if (priorTo2_13(scalaVersion.value)) {
        Seq.empty
      } else {
        Seq("-Ymacro-annotations")
      }
    }
  )
}
