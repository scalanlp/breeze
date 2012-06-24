import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import de.johoop.jacoco4sbt._
import JacocoPlugin._



object BuildSettings {
  val buildOrganization = "org.scalanlp"
  val buildScalaVersion = "2.9.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq("-optimize","-deprecation", "-Ydependent-method-types"),
    resolvers ++= Seq(
      "Breeze Maven2" at "http://repo.scalanlp.org/repo"
      // "ondex" at "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public"
    )
  )
}


object BreezeBuild extends Build {
  import BuildSettings._

  val p = System.getProperties();
  p.setProperty("log.level","WARN")

  val paranamer = "com.thoughtworks.paranamer" % "paranamer" % "2.2"

  val netlib = "com.googlecode.netlib-java" % "netlib-java" % "0.9.3"

  val commonDeps = Seq(paranamer, netlib)

  def testDependencies = libraryDependencies <++= (scalaVersion) {
    sv =>
      Seq(
        sv match {
          case "2.9.2" => "org.scala-tools.testing" % "scalacheck_2.9.1" % "1.9" % "test"
          case _       => "org.scala-tools.testing" % "scalacheck" % "1.9" % "test"
        },
        "org.scalatest" % "scalatest_2.9.0" % "1.8" % "test",
        "junit" % "junit" % "4.5" % "test"
      )
  }

  //
  // subprojects
  //

  lazy val breeze = Project("breeze", file("."), settings = buildSettings ++ jacoco.settings) aggregate (math,process,learn,graphs) dependsOn (math,process,learn,graphs)
  lazy val math = Project("breeze-math",file("math"), settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ testDependencies ++ assemblySettings ++ jacoco.settings)
  lazy val process = Project("breeze-process",file("process"), settings =  buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ testDependencies ++ assemblySettings ++ jacoco.settings) dependsOn(math)
  lazy val learn = Project("breeze-learn",file("learn") , settings = buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ testDependencies++ assemblySettings ++ jacoco.settings) dependsOn(math,process)
  lazy val graphs = Project("breeze-graphs",file("graphs"), settings = buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ testDependencies) dependsOn(math,process)
  lazy val examples = Project("breeze-examples",file("examples"), settings = buildSettings ++ Seq (libraryDependencies ++= commonDeps) ++ testDependencies) dependsOn(math,learn,graphs,process)
}

