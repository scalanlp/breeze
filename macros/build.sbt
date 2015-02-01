organization := "org.scalanlp"

name := "breeze-macros"

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  Resolver.sonatypeRepo("releases")
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.5" % "test"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case x if x.startsWith("2.10") =>
      deps :+ ("org.scalamacros" %% "quasiquotes" % "2.0.0-M8")
    case _ => deps
  }
}

scalaVersion := "2.11.4"

crossScalaVersions  := Seq("2.11.4", "2.10.4")

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
  "org.scalatest"  %% "scalatest"  % "2.1.3"  % "test"
)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

scalacOptions ++= Seq("-deprecation", "-language:_", "-optimize")

javaOptions += "-Xmx2g"

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.0.1" cross CrossVersion.full)

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



  pomIncludeRepository := { _ => false }

publishMavenStyle := true


  publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  }
