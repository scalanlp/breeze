name := "breeze-viz"

version := "0.9"

organization := "org.scalanlp"

scalaVersion := "2.11.1"

crossScalaVersions  := Seq("2.11.1", "2.10.3")

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
    "junit" % "junit" % "4.5" % "test",
    "jfree" % "jcommon" % "1.0.16",
    "jfree" % "jfreechart" % "1.0.13",
    "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.3.1", // for eps gen
    // "org.apache.xmlgraphics" % "batik-dom" % "1.7",    // for svg gen
    // "org.apache.xmlgraphics" % "batik-svggen" % "1.7", // for svg gen
    "com.lowagie" % "itext" % "2.1.5" intransitive(),  // for pdf gen
    "org.scalanlp" %% "breeze" % "0.9"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case "2.9.2" =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.4.RC2" % "test"))
    case x if x.startsWith("2.8") =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.3" % "test")
            :+ ("org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"))
    case _       =>
     (deps :+ ("org.scalacheck" %% "scalacheck" % "1.11.4" % "test")
           :+ ("org.scalatest" %% "scalatest" % "2.1.6" % "test"))
  }
}


libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")


scalacOptions ++= Seq("-deprecation", "-language:_", "-optimize")

javaOptions += "-Xmx2g"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full)

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
      <connection>scm:git:git@github.com:dlwh/breeze-viz.git</connection>
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

