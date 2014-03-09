organization := "org.scalanlp"

// lazy val breeze = project in file("core")
name := "breeze-benchmark"

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


libraryDependencies ++= Seq(
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

lazy val key: sbt.AttributeKey[Boolean] = AttributeKey[Boolean]("javaOptionsPatched")

onLoad in Global ~= { previous => state =>
  val k = key.asInstanceOf[sbt.AttributeKey[Boolean]]
  previous {
    state.get(k) match {
      case None =>
        // get the runtime classpath, turn into a colon-delimited string
        // return a state with javaOptionsPatched = true and javaOptions set correctly
        val extracted = Project.extract(state)
        val set = for(proj <- extracted.structure.allProjectRefs) yield {
          val classPath = Project.runTask(fullClasspath in Runtime in proj, state).get._2.toEither.right.get.files.mkString(":")
          javaOptions in (proj, Runtime) ++= Seq("-cp", classPath)
        }
        extracted.append(set, state.put(k, true))
        case Some(_) =>
        state // the javaOptions are already patched
    }
  }
}

pomIncludeRepository := { _ => false }

fork := true

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




resolvers ++= Seq(
    Resolver.mavenLocal,
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases")
    )

testOptions in Test += Tests.Argument("-oDF")


publish := ()

publishLocal := ()

publishArtifact := false


