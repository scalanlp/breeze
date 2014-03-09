organization := "org.scalanlp"

name := "breeze"

lazy val (root, natives, benchmark) = {
  var root = project.in(file("."))
  var natives = project.in(file("natives"))
  var benchmark = project.in(file("benchmark")).dependsOn(root, natives)
  root = root.aggregate(natives, benchmark).settings(aggregate in test := false, aggregate in compile := false)
  natives = natives.dependsOn(root)
  (root, natives, benchmark)
}


scalaVersion := "2.10.3"

addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full)

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

scalacOptions ++= Seq("-deprecation","-language:_")

// scalacOptions in (Compile, console) += "-Xlog-implicits"


  javacOptions ++= Seq("-target", "1.6", "-source","1.6")


libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze-macros" % "0.3-SNAPSHOT" % "compile",
  "com.thoughtworks.paranamer" % "paranamer" % "2.2",
  "com.github.fommil.netlib" % "core" % "1.1.2",
  "net.sourceforge.f2j" % "arpack_combined_all" % "0.1",
  "net.sf.opencsv" % "opencsv" % "2.3",
  "com.github.rwl" % "jtransforms" % "2.4.0",
  "org.apache.commons" % "commons-math3" % "3.2",
  "org.spire-math" %% "spire" % "0.7.1",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  "org.scalatest" %% "scalatest" % "2.0.M5b" % "test",
  "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.0-beta9" % "test",
  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta9" % "test",
  "org.apache.logging.log4j" % "log4j-api" % "2.0-beta9" % "test",
  "com.chuusai" % "shapeless_2.10.3" % "2.0.0-M1" % "test"
)

// see https://github.com/typesafehub/scalalogging/issues/23
testOptions in Test += Tests.Setup(classLoader =>
try {
  classLoader
    .loadClass("org.slf4j.LoggerFactory")
    .getMethod("getLogger", classLoader.loadClass("java.lang.String"))
    .invoke(null, "ROOT")
    } catch {
      case e: Exception => 
    }
)

resolvers ++= Seq(
    Resolver.mavenLocal,
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases")
    )

testOptions in Test += Tests.Argument("-oDF")


