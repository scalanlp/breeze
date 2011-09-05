import sbt._
import java.util.jar.Attributes.Name._


class Project(info: ProjectInfo) extends DefaultProject(info) {
  // 
  // repositories
  //

  val ScalaNLPRepo = "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
  val OndexRepo = "ondex" at "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public"
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  val ivyLocal = "ivy local" at "file://" + Path.userHome +".ivy/local/"
  override def ivyRepositories = Resolver.withDefaultResolvers(repositories.toSeq, false)

  //
  // subprojects
  //

  lazy val data = project("data","data", new Data(_));
  lazy val learn = project("learn","learn",new Learn(_), data);
  lazy val graphs = project("graphs","graphs",new Graphs(_), data);

  class Data(info: ProjectInfo) extends DefaultProject(info) {
    val paranamer = "com.thoughtworks.paranamer" % "paranamer" % "2.2"
    val JLine = "jline" % "jline" % "0.9.94"
    val Scalala = "org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT";
    val ScalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test"
    //val casbah = "com.mongodb.casbah" % "casbah_2.9.0-1" % "2.1.5.0"
    val ScalaTest = buildScalaVersion match {
    case "2.9.0"     => "org.scalatest" % "scalatest" % "1.4.RC2" % "test"
    case "2.8.1"     => "org.scalatest" % "scalatest" % "1.3" % "test"
    case x           => error("Unsupported Scala version " + x)
  }

    val JUnit = "junit" % "junit" % "4.5" % "test"
  }

  class Learn(info: ProjectInfo) extends DefaultProject(info) {
    lazy val logLevel = system[String]("log.level")
    // suprress INFO for tests
    logLevel.update("WARN")
    val paranamer = "com.thoughtworks.paranamer" % "paranamer" % "2.2"
    val JLine = "jline" % "jline" % "0.9.94"
    val Scalala = "org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT";
    val ScalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test"
      val ScalaTest = buildScalaVersion match {
    case "2.9.0"     => "org.scalatest" % "scalatest" % "1.4.RC2" % "test"
    case "2.8.1"     => "org.scalatest" % "scalatest" % "1.3" % "test"
    case x           => error("Unsupported Scala version " + x)
  }
    val JUnit = "junit" % "junit" % "4.5" % "test"
  }

  class Graphs(info: ProjectInfo) extends DefaultProject(info) {
    val paranamer = "com.thoughtworks.paranamer" % "paranamer" % "2.2"
    val JLine = "jline" % "jline" % "0.9.94"
    val Scalala = "org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT";
    val ScalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test"

    val ScalaTest = buildScalaVersion match {
      case "2.9.0"     => "org.scalatest" % "scalatest" % "1.4.RC2" % "test"
      case "2.8.1"     => "org.scalatest" % "scalatest" % "1.3" % "test"
      case x           => error("Unsupported Scala version " + x)
    }
    val JUnit = "junit" % "junit" % "4.5" % "test"
  }







  //
  // configuration
  //
  
  override def mainClass: Option[String] = Some("scalala.ScalalaConsole")

  override def compileOptions =
    Optimise :: Deprecation ::
    target(Target.Java1_5) ::
    Unchecked :: CompileOption("-no-specialization") ::
    super.compileOptions.toList

  override def packageOptions = ManifestAttributes(
    MAIN_CLASS -> "scalala.ScalalaConsole",
    IMPLEMENTATION_TITLE -> "Scalala",
    IMPLEMENTATION_URL -> "http://scalala.org/",
    IMPLEMENTATION_VENDOR -> "scalala.org",
    SEALED -> "true") :: Nil

  override def managedStyle = ManagedStyle.Maven

  override def packageDocsJar = defaultJarPath("-javadoc.jar")

  override def packageSrcJar = defaultJarPath("-sources.jar")

  override def packageTestSrcJar = defaultJarPath("-test-sources.jar")

  lazy val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)

  lazy val docsArtifact = Artifact(artifactID, "docs", "jar", Some("javadoc"), Nil, None)

  override def compileOrder = CompileOrder.JavaThenScala

  override def packageToPublishActions =
    super.packageToPublishActions ++ Seq(packageDocs, packageSrc, packageTestSrc)
  
  /*
  override def allDependencyJars = (
    super.allDependencyJars +++ 
    Path.fromFile(buildScalaInstance.compilerJar) +++ 
    Path.fromFile(buildScalaInstance.libraryJar)
  )

*/
/*
  override def proguardOptions = List(
    "-keep class scalala.** { *; }",
    "-keep class org.jfree.** { *; }",
    proguardKeepMain("scalala.ScalalaConsole$"),
    proguardKeepMain("scala.tools.nsc.MainGenericRunner"),
    "-dontoptimize",
    "-dontobfuscate", 
    proguardKeepLimitedSerializability,
    proguardKeepAllScala,
    "-keep class ch.epfl.** { *; }",
    "-keep interface scala.ScalaObject"
  )
  */

  //
  // publishing
  //  
  val publishToRepoName = "Sonatype Nexus Repository Manager"
  val publishTo = {
    val repoUrl = "http://nexus.scala-tools.org/content/repositories/" +
      (if (version.toString.endsWith("-SNAPSHOT")) "snapshots" else "releases")
    publishToRepoName at repoUrl
  }

  lazy val publishUser = system[String]("build.publish.user")
  lazy val publishPassword = system[String]("build.publish.password")

  (publishUser.get, publishPassword.get) match {
    case (Some(u), Some(p)) =>
      Credentials.add(publishToRepoName, "nexus.scala-tools.org", u, p)
    case _ =>
      Credentials(Path.userHome / ".ivy2" / ".credentials", log)
  }


}

