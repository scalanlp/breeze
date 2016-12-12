Common.commonSettings

name := "breeze-macros"

libraryDependencies := {
  val sv = scalaVersion.value
  val deps = libraryDependencies.value
  sv match {
    case x if x.startsWith("2.10") =>
      deps :+ ("org.scalamacros" %% "quasiquotes" % "2.1.0")
    case _ => deps
  }
}

libraryDependencies += "org.scala-lang" % "scala-reflect" % s"${scalaVersion.value}"

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

