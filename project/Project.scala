import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._


object BuildSettings {
  val buildOrganization = "org.scalanlp"
  val buildScalaVersion = "2.10.2"

  val scalaVersionRegex = "(\\d+)\\.(\\d+).*".r


  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    resolvers ++= Seq(
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
    ),
    crossScalaVersions := Seq("2.10.2"),
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

  val paranamer = "com.thoughtworks.paranamer" % "paranamer" % "2.2"
  val netlib = "com.googlecode.netlib-java" % "netlib-java" % "0.9.3"
  val jblas = "org.jblas" % "jblas" % "1.2.3"
  val liblinear = "de.bwaldvogel" % "liblinear" % "1.8"
  val opencsv = "net.sf.opencsv" % "opencsv" % "2.3"
  val logging = "com.typesafe" %% "scalalogging-log4j" % "1.0.1"
  val log4j =  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta4"
  val jtransforms = "com.github.rwl" % "jtransforms" % "2.4.0"

  val commonsMath =  "org.apache.commons" % "commons-math3" % "3.2"
            


  val coreDeps = Seq(paranamer, opencsv, logging, log4j)
  val commonDeps = Seq(paranamer, netlib, jblas, commonsMath, jtransforms)
  val vizDeps = Seq(
    "jfree" % "jcommon" % "1.0.16",
    "jfree" % "jfreechart" % "1.0.13",
    "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.3.1", // for eps gen
    // "org.apache.xmlgraphics" % "batik-dom" % "1.7",    // for svg gen
    // "org.apache.xmlgraphics" % "batik-svggen" % "1.7", // for svg gen
    "com.lowagie" % "itext" % "2.1.5" intransitive()  // for pdf gen
  )

  val benchmarkDeps = Seq(
    "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
    "com.google.code.gson" % "gson" % "1.7.1",
    "com.google.caliper" % "caliper" % "0.5-rc1"
  )
  

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


  def patchclasspath = Command.command("patchclasspath") { (state: State) =>
    val extracted = Project.extract(state)
    import extracted._
    println(currentProject.id)
    val classPath = Project.runTask(fullClasspath in Runtime, state).get._2.toEither.right.get.files.mkString(":")
    println(classPath)
    // return a state with javaOptionsPatched = true and javaOptions set correctly
    Project.extract(state).append(Seq(javaOptions ++= Seq("-cp", classPath)), state.put(caliperLoopKey, true))
  }

  val caliperFixForBenchmark = (

    // we need to add the runtime classpath as a "-cp" argument to the `javaOptions in run`, otherwise caliper
    // will not see the right classpath and die with a ConfigurationException
    // unfortunately `javaOptions` is a SettingsKey and `fullClasspath in Runtime` is a TaskKey, so we need to
    // jump through these hoops here in order to feed the result of the latter into the former
    onLoad in Global ~= { previous => state =>
      previous {
        state.get(caliperLoopKey) match {
          case None =>
            println("!"+ Project.extract(state).currentProject.id)
            // get the runtime classpath, turn into a colon-delimited string
            val classPath = Project.runTask(fullClasspath in Runtime, state).get._2.toEither.right.get.files.mkString(":")
            // return a state with javaOptionsPatched = true and javaOptions set correctly
            Project.extract(state).append(Seq(javaOptions ++= Seq("-cp", classPath)), state.put(caliperLoopKey, true))
            
          case Some(_) => // the javaOptions are already patched
            println("?"+Project.extract(state).currentProject.id)
            state
        }
      }
    }

  )

 val caliperLoopKey = AttributeKey[Boolean]("javaOptionsPatched") // attribute caliperLoopKey to prevent circular onLoad hook


  //
  // subprojects
  //

  lazy val breeze = Project("breeze", file("."), settings = buildSettings) aggregate (core, math,viz) dependsOn (core, math, viz)
  lazy val core = Project("breeze-core",file("core"), settings =  buildSettings ++ Seq (libraryDependencies ++= coreDeps) ++ testDependencies ++ assemblySettings)
  lazy val math = Project("breeze-math",file("math"), settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ testDependencies ++ assemblySettings) dependsOn(core)
  lazy val benchmark = Project("breeze-benchmark",file("benchmark"), settings = (buildSettings :+ (fork in run := true) :+ (commands += patchclasspath)) ++ Seq (libraryDependencies ++= (commonDeps ++ benchmarkDeps)) ++ testDependencies) dependsOn(math)
  lazy val viz =  Project("breeze-viz", file("viz"),  settings =  buildSettings ++ Seq (libraryDependencies ++= (commonDeps ++ vizDeps)) ++ testDependencies ++ assemblySettings) dependsOn(core, math)

val _projects: Seq[ProjectReference] = Seq(math,viz,core)
  lazy val doc = Project("doc", file("doc"))
      .settings((buildSettings ++ Seq(
        version := "1.0",
        unmanagedSourceDirectories in Compile <<= (_projects map (unmanagedSourceDirectories in _ in Compile)).join.apply {(s) => s.flatten} 
  ) ++ Seq ( libraryDependencies ++= (coreDeps ++ commonDeps ++ vizDeps)):_*))
}

