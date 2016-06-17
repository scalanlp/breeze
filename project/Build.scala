import sbt._
import Keys._

object Common {
  val crossScalaVersions = Seq("2.11.8", "2.10.6")
  val scalaVersion = crossScalaVersions.head

  val commonSettings = Seq (
    organization := "org.scalanlp",
    Keys.scalaVersion := Common.scalaVersion,
    Keys.crossScalaVersions  := Common.crossScalaVersions,

    scalacOptions ++= Seq("-deprecation","-language:_"),

    javacOptions ++= Seq("-target", "1.6", "-source","1.6"),

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
  <scm>
    <url>git@github.com:scalanlp/breeze.git</url>
    <connection>scm:git:git@github.com:scalanlp/breeze.git</connection>
  </scm>
  <developers>
    <developer>
      <id>dlwh</id>
      <name>David Hall</name>
      <url>http://www.dlwh.org/</url>
    </developer>
  </developers>,
    publishMavenStyle := true,

    publishTo <<= isSnapshot { (yes: Boolean) =>
      val nexus = "https://oss.sonatype.org/"
      if (yes)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },

    publishArtifact in Test := false,

    pomIncludeRepository := { _ => false }
  )
}
