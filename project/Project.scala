import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._


object BuildSettings {
  val buildOrganization = "org.scalanlp"
  val buildScalaVersion = "2.10.3"

  val scalaVersionRegex = "(\\d+)\\.(\\d+).*".r


  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.typesafeRepo("releases")
    ),
addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full),
    crossScalaVersions := Seq("2.10.3"),
  publishMavenStyle := true,
  publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) 
      Some("snapshots" at nexus + "content/repositories/snapshots") 
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := (
    <url>http://scalanlp.org/</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:dlwh/breeze.git</url>
      <connection>scm:git:git@github.com:dlwh/breeze.git</connection>
    </scm>
    <developers>
      <developer>
        <id>dlwh</id>
        <name>David Hall</name>
        <url>http://cs.berkeley.edu/~dlwh/</url>
      </developer>
    </developers>),
  scalacOptions ++= Seq("-optimize","-deprecation","-language:_"),
    javacOptions ++= Seq("-target", "1.6", "-source","1.6")
  )
}


object BreezeBuild extends Build {
  import BuildSettings._

  val p = System.getProperties();
  p.setProperty("log.level","WARN")

  val breezeMacros = "org.scalanlp" %% "breeze-macros" % "0.1"
  val paranamer = "com.thoughtworks.paranamer" % "paranamer" % "2.2"
  val netlib = "com.github.fommil.netlib" % "all" % "1.1-SNAPSHOT" pomOnly()
  val liblinear = "de.bwaldvogel" % "liblinear" % "1.8"
  val opencsv = "net.sf.opencsv" % "opencsv" % "2.3"
  val logging = "com.typesafe" %% "scalalogging-log4j" % "1.0.1"
  val log4j =  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta4"
  val jtransforms = "com.github.rwl" % "jtransforms" % "2.4.0"

  val commonsMath =  "org.apache.commons" % "commons-math3" % "3.2"

  val coreDeps = Seq(paranamer, opencsv, logging, log4j)
  val commonDeps = Seq(paranamer, netlib, commonsMath, jtransforms, breezeMacros)

  def testDependencies = libraryDependencies <++= (scalaVersion) {
    sv =>
      Seq(
        sv match {
          case "2.9.2" => "org.scala-tools.testing" % "scalacheck_2.9.1" % "1.9" % "test"
          case "2.10.0-RC1" => "org.scalacheck" % "scalacheck_2.10.0-RC1" % "1.10.0" % "test"
          case _       => "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
        },
        sv match {
          case "2.9.2" => "org.scalatest" %% "scalatest" % "2.0.M5" % "test"
          case _       => "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"
        },
        "junit" % "junit" % "4.5" % "test"
      )
  }

  //
  // subprojects
  //

  lazy val breeze = Project("breeze", file("."), settings = buildSettings) aggregate (core, math) dependsOn (core, math)
  lazy val core = Project("breeze-core",file("core"), settings =  buildSettings ++ Seq (libraryDependencies ++= coreDeps) ++ testDependencies ++ assemblySettings)
  lazy val math = Project("breeze-math",file("math"), settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ testDependencies ++ assemblySettings) dependsOn(core)

}

