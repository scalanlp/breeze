name := "breeze"

Common.commonSettings

libraryDependencies ++= Seq(
  "dev.ludovic.netlib" % "blas" % "2.0.0",
  "dev.ludovic.netlib" % "lapack" % "2.0.0",
  "dev.ludovic.netlib" % "arpack" % "2.0.0",
  "net.sourceforge.f2j" % "arpack_combined_all" % "0.1",
  "net.sf.opencsv" % "opencsv" % "2.3",
  "com.github.wendykierp" % "JTransforms" % "3.1",
  "org.apache.commons" % "commons-math3" % "3.2",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.typelevel" %% "spire" % "0.17.0",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.0-beta9" % "test",
  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta9" % "test",
  "org.apache.logging.log4j" % "log4j-api" % "2.0-beta9" % "test",
  "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.1"
)

// see https://github.com/typesafehub/scalalogging/issues/23
testOptions in Test += Tests.Setup(classLoader =>
  try {
    classLoader
      .loadClass("org.slf4j.LoggerFactory")
      .getMethod("getLogger", classLoader.loadClass("java.lang.String"))
      .invoke(null, "ROOT")
  } catch {
    case _: Exception =>
})

fork := true

javaOptions := Seq("-Xmx4g")

unmanagedSourceDirectories in Compile += {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11|12)) => baseDirectory.value / "src" / "main" / "scala_2.11_2.12"
    case Some((2, 13)) => baseDirectory.value / "src" / "main" / "scala_2.13"
    case _ => ???
  }
}
