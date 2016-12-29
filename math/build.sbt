name := "breeze"

Common.commonSettings

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "com.github.fommil.netlib" % "core" % "1.1.2",
  "net.sourceforge.f2j" % "arpack_combined_all" % "0.1",
  "net.sf.opencsv" % "opencsv" % "2.3",
  "com.github.rwl" % "jtransforms" % "2.4.0",
  "org.apache.commons" % "commons-math3" % "3.2",
  "org.spire-math" %% "spire" % "0.13.0",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.0-beta9" % "test",
  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta9" % "test",
  "org.apache.logging.log4j" % "log4j-api" % "2.0-beta9" % "test"
)

libraryDependencies := {
  val sv = scalaVersion.value
  val deps = libraryDependencies.value
  sv match {
    case x if x startsWith "2.10" =>
      deps :+ compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
    case _ =>
      deps
  }
}

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 12 =>
      libraryDependencies.value ++ Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.6" % "test"
      )
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value ++ Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.6" % "test"
      )
    case _ =>
      libraryDependencies.value ++ Seq()
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
    case _: Exception =>
  }
)

fork in Compile := true

javaOptions := Seq("-Xmx4g")

