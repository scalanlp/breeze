import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._


object BuildSettings {
  val buildOrganization = "org.scalanlp"
  val buildVersion      = "0.5-SNAPSHOT"
  val buildScalaVersion = "2.9.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq("-no-specialization","-optimize","-deprecation")
  )
}



object ScalanlpBuild extends Build {
  import BuildSettings._
  // 
  // repositories
  //

  val ScalaNLPRepo = "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
  val OndexRepo = "ondex" at "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public"
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  val ivyLocal = "ivy local" at "file://" + Path.userHome +".ivy/local/"

  val repos = Seq(ScalaNLPRepo,OndexRepo,scalaToolsSnapshots,ivyLocal)

  val p = System.getProperties();
  p.setProperty("log.level","WARN")
  

  // variosu deps
  val paranamer = "com.thoughtworks.paranamer" % "paranamer" % "2.2"
  val Scalala = "org.scalala" %% "scalala" % "1.0.0.RC3-SNAPSHOT";
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

  val commonDeps = Seq(paranamer,Scalala,ScalaCheck,ScalaTest,JUnit)

  //
  // subprojects
  //

  lazy val core = Project ( "core", file("."), settings = buildSettings) aggregate (data,learn,graphs) dependsOn (data,learn,graphs)

  lazy val data = Project("scalanlp-data",file("data"), settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ assemblySettings) 
  lazy val learn = Project("scalanlp-learn",file("learn") ,  settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ assemblySettings) dependsOn(data)
  lazy val graphs = Project("scalanlp-graphs",file("graphs"),  settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps)) dependsOn(data)
  lazy val examples = Project("scalanlp-examples",file("examples"),  settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps)) dependsOn(learn,graphs,data)

}

