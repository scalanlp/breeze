name := "breeze-viz"

organization := "org.scalanlp"

resolvers ++= Seq(
  "Sonatype Snapshots".at("https://oss.sonatype.org/content/repositories/snapshots/")
)

libraryDependencies ++= Seq(
  "org.jfree" % "jfreechart" % "1.5.0",
  "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.3.1", // for eps gen
  // "org.apache.xmlgraphics" % "batik-dom" % "1.7",    // for svg gen
  // "org.apache.xmlgraphics" % "batik-svggen" % "1.7", // for svg gen
  ("com.lowagie" % "itext" % "2.1.5").intransitive() // for pdf gen
)

crossScalaVersions := Common.buildCrossScalaVersions

scalaVersion := Common.buildScalaVersion

//libraryDependencies += "org.scala-lang" % "scala-reflect" % s"${scalaVersion.value}"

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

scalacOptions ++= Seq("-deprecation", "-language:_")

javaOptions += "-Xmx2g"

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
        <url>http://cs.berkeley.edu/~dlwh/</url>
      </developer>
    </developers>

pomIncludeRepository := { _ =>
  false
}

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots".at(nexus + "content/repositories/snapshots"))
  else
    Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
}
