import sbt._
import Defaults._
import Keys._

object ApplicationBuild extends Build {

  lazy val commonSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalanlp",
    scalaVersion := "2.10.3",
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full),
    resolvers ++= Dependencies.resolvers,
    libraryDependencies ++= Dependencies.common,
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
    scalacOptions ++= Seq("-deprecation","-language:_"),
    javacOptions ++= Seq("-target", "1.6", "-source","1.6"),
    // see https://github.com/typesafehub/scalalogging/issues/23
    testOptions in Test += Tests.Setup(classLoader =>
      classLoader
        .loadClass("org.slf4j.LoggerFactory")
        .getMethod("getLogger", classLoader.loadClass("java.lang.String"))
        .invoke(null, "ROOT")
    ),
    testOptions in Test += Tests.Argument("-oDF")
  )

  lazy val (root,natives) = {
    var r = project.in(file("."))
    var n = project.in(file("natives"))
    r = r.aggregate(n).settings(aggregate in test := false, aggregate in compile := false)
    n = n.dependsOn(r)
    (r -> n)
  }

  lazy val breeze = Project("breeze", file("breeze"), settings = commonSettings ++ Seq(name := "breeze"))

  lazy val benchmark: Project = Project("benchmark", file("benchmark")).
    settings(benchmarkSettings: _*).
    dependsOn(breeze)

  lazy val benchmarkSettings = Seq(
    name := "breeze-benchmark",
    javaOptions in run += "-Xmx4G",// raise memory limits here if necessary
    fork in run := true,
    libraryDependencies ++= (Dependencies.common ++ Dependencies.benchmark),
    resolvers ++= Dependencies.resolvers,
    onLoad in Global ~= { previous => state =>
      previous {
        state.get(key) match {
          case None =>
            // get the runtime classpath, turn into a colon-delimited string
            val classPath = Project.runTask(fullClasspath in Runtime in benchmark, state).get._2.toEither.right.get.files.mkString(":")
            // return a state with javaOptionsPatched = true and javaOptions set correctly
            Project.extract(state).append(Seq(javaOptions in (benchmark, run) ++= Seq("-cp", classPath)), state.put(key, true))
          case Some(_) =>
            state // the javaOptions are already patched
        }
      }
    }
  ) ++ noPublish


  object Dependencies {
    val common = Seq(
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
    val benchmark = Seq(
      // comparisons
      "org.apfloat" % "apfloat" % "1.6.3",
      "org.jscience" % "jscience" % "4.3.1",
      "org.apache.commons" % "commons-math3" % "3.2",
      // thyme
      "ichi.bench" % "thyme" % "0.1.0" from "http://plastic-idolatry.com/jars/thyme-0.1.0.jar",
      // caliper stuff
      "com.google.guava" % "guava" % "r09",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1"
    )
    val resolvers = Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.typesafeRepo("releases")
    )
  }

  lazy val key = AttributeKey[Boolean]("javaOptionsPatched")

  lazy val noPublish = Seq(
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )

}
