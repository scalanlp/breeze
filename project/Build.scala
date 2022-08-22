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

  val buildCrossScalaVersions = Seq("3.1.3", "2.12.15", "2.13.8")

  lazy val buildScalaVersion = buildCrossScalaVersions.head

  val commonSettings = Seq(
    organization := "org.scalanlp",
    scalaVersion := buildScalaVersion,
    crossScalaVersions := buildCrossScalaVersions,
    scalacOptions ++= Seq("-deprecation", "-language:_"),
    javacOptions ++= Seq("-target", "1.8", "-source", "1.8"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),

    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.typesafeRepo("releases")
    ),
    testOptions in Test += Tests.Argument("-oDF"),

    // test dependencies
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
      "org.scalatest" %% "scalatest-funsuite" % "3.2.9" % "test",
      "org.scalatest" %% "scalatest-wordspec" % "3.2.9" % "test",
      "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % "test",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % "test"
    ),
    libraryDependencies ++= {
      if (priorTo2_13(scalaVersion.value)) {
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch),
        )
      } else {
        Seq(

        )
      }
    },
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, minor)) if minor < 13 => Seq.empty
        case Some((2, 13)) =>
          Seq("-Ymacro-annotations", "-language:implicitConversions")
        case _ =>
          Seq("-language:implicitConversions")
      }
    },

    // stuff related to publishing
    publishArtifact in Test := false,
    pomIncludeRepository := { _ =>
      false
    },
    sonatypeProfileName := "org.scalanlp",
    publishMavenStyle := true,
    licenses := Seq("Apache Public License 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")),
    publishTo := sonatypePublishTo.value,
    sonatypeProjectHosting := Some(GitHubHosting("scalanlp", "breeze", "David Hall", "david.lw.hall@gmail.com")),

    unmanagedSourceDirectories in Compile ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11|12)) => Seq(
          baseDirectory.value / "src" / "main" / "scala_2.11_2.12",
          baseDirectory.value / "src" / "main" / "scala_2",
        )
        case Some((2, 13)) => Seq(
          baseDirectory.value / "src" / "main" / "scala_2",
          baseDirectory.value / "src" / "main" / "scala_2.13+"
        )
        case Some( (3, _)) => Seq(
          baseDirectory.value / "src" / "main" / "scala_2.13+",
          baseDirectory.value / "src" / "main" / "scala_3"
        )
        case _ => ???
      }
    }, 
    // TODO: remove when possibl`e
   // publishArtifact in (Compile, packageDoc) := {
    //  CrossVersion.partialVersion(scalaVersion.value) match {
     //   case Some( (3, _)) => false
    //    case _ => true
//      
     // }
   // }
  ) ++ breezeCodegenSettings

}
