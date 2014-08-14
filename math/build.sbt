organization := "org.scalanlp"

name := "breeze"

scalaVersion := "2.11.1"

crossScalaVersions  := Seq("2.11.1", "2.11.0", "2.10.3")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0-M1" cross CrossVersion.full)

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

scalacOptions ++= Seq("-deprecation","-language:_")//, "-no-specialization")

// scalacOptions in (Compile, console) += "-Xlog-implicits"

javacOptions ++= Seq("-target", "1.6", "-source","1.6")

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze-macros" % "0.3.1" % "compile",
  "com.github.fommil.netlib" % "core" % "1.1.2",
  "net.sourceforge.f2j" % "arpack_combined_all" % "0.1",
  "net.sf.opencsv" % "opencsv" % "2.3",
  "com.github.rwl" % "jtransforms" % "2.4.0",
  "org.apache.commons" % "commons-math3" % "3.2",
  "org.spire-math" %% "spire" % "0.7.4",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.1" % "test",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.0-beta9" % "test",
  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta9" % "test",
  "org.apache.logging.log4j" % "log4j-api" % "2.0-beta9" % "test"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case x if x startsWith "2.10" =>
      (deps :+ ("com.chuusai" %% "shapeless" % "2.0.0" % "test" cross CrossVersion.full))
    case x if x.startsWith("2.11") =>
      (deps :+ ("com.chuusai" %% "shapeless" % "2.0.0" % "test"  ))
    case _       =>
      deps
  }
}

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

fork in Test := true

javaOptions := Seq("-Xmx4g")

jacoco.settings
