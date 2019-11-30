import sbt.Keys._
import sbt._

object Common {

  def priorTo2_13(scalaVersion: String): Boolean = {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor < 13 => true
      case _ => false
    }
  }

  val buildCrossScalaVersions = Seq("2.12.8", "2.11.12", "2.13.0")

  lazy val buildScalaVersion = buildCrossScalaVersions.head

  val commonSettings = Seq(
    organization := "org.scalanlp",
    scalaVersion := buildScalaVersion,
    crossScalaVersions := buildCrossScalaVersions,
    scalacOptions ++= Seq("-deprecation", "-language:_"),
    javacOptions ++= Seq("-target", "1.7", "-source", "1.7"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
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
        Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch))
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
    }
  )
}
