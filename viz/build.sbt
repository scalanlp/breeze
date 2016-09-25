name := "breeze-viz"

organization := "org.scalanlp"

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
    "jfree" % "jcommon" % "1.0.16",
    "jfree" % "jfreechart" % "1.0.13",
    "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.3.1", // for eps gen
    // "org.apache.xmlgraphics" % "batik-dom" % "1.7",    // for svg gen
    // "org.apache.xmlgraphics" % "batik-svggen" % "1.7", // for svg gen
    "com.lowagie" % "itext" % "2.1.5" intransitive()  // for pdf gen
)

scalaVersion := Common.scalaVersion

crossScalaVersions  := Common.crossScalaVersions

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

scalacOptions ++= Seq("-deprecation", "-language:_", "-optimize")

javaOptions += "-Xmx2g"

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
      <url>git@github.com:scalanlp/breeze.git</url>
      <connection>scm:git:git@github.com:scalanlp/breeze-viz.git</connection>
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

