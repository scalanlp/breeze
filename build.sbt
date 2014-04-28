organization := "org.scalanlp"

name := "breeze-parent"

lazy val root = project.in( file(".") )
    .aggregate(math, natives).settings(aggregate in test := false, aggregate in compile := false)

lazy val math = project.in( file("math"))

lazy val natives = project.in(file("natives")).dependsOn(math)

lazy val benchmark = project.in(file("benchmark")).dependsOn(math, natives)

scalaVersion := "2.11.0"

crossScalaVersions  := Seq("2.11.0", "2.10.3")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.0.0-M8" cross CrossVersion.full)

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

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
  </developers>)

