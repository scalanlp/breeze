import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._


object BuildSettings {
  val buildOrganization = "org.scalanlp"
  val buildVersion      = "0.1-SNAPSHOT"
  val buildScalaVersion = "2.9.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq("-optimize","-deprecation", "-Ydependent-method-types")
  )
}


object BreezeBuild extends Build {
  import BuildSettings._
  // 
  // repositories
  //

  val BreezeRepo = "Breeze Maven2" at "http://repo.scalanlp.org/repo"
  val OndexRepo = "ondex" at "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public"
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  val ivyLocal = "ivy local" at "file://" + Path.userHome +".ivy/local/"

  val repos = Seq(BreezeRepo,OndexRepo,scalaToolsSnapshots,ivyLocal)

  val p = System.getProperties();
  p.setProperty("log.level","WARN")
  
  // various deps
  val paranamer = "com.thoughtworks.paranamer" % "paranamer" % "2.2"
  val ScalaCheck = buildScalaVersion match {
    case "2.9.1" => "org.scala-tools.testing" % "scalacheck_2.9.0" % "1.8" % "test"
    case _       => "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test"
  }

  val ScalaTest = buildScalaVersion match {
    case "2.9.1"     => "org.scalatest" % "scalatest" % "1.4.RC2" % "test"
    case "2.8.1"     => "org.scalatest" % "scalatest" % "1.3" % "test"
    case x           => error("Unsupported Scala version " + x)
  }
  val JUnit = "junit" % "junit" % "4.5" % "test"
  val netlib = "com.googlecode.netlib-java" % "netlib-java" % "0.9.3"


  val commonDeps = Seq(paranamer,ScalaCheck,ScalaTest,JUnit, netlib)

  //
  // subprojects
  //

  lazy val breeze = Project ( "breeze", file("."), settings = buildSettings) aggregate (math,data,learn,graphs) dependsOn (math,data,learn,graphs)

  lazy val math = Project("breeze-math",file("math"), settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ assemblySettings) 
  lazy val data = Project("breeze-data",file("data"), settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ assemblySettings) dependsOn(math)
  lazy val learn = Project("breeze-learn",file("learn") ,  settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ assemblySettings) dependsOn(math,data)
  lazy val graphs = Project("breeze-graphs",file("graphs"),  settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps)) dependsOn(math,data)
  lazy val examples = Project("breeze-examples",file("examples"),  settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps)) dependsOn(math,learn,graphs,data)
}

