Common.commonSettings

name := "breeze-macros"

libraryDependencies := {
  val sv = scalaVersion.value
  val deps = libraryDependencies.value
  sv match {
    case x if x.startsWith("2.10") =>
      deps :+ ("org.scalamacros" %% "quasiquotes" % "2.1.0") :+ "org.scala-lang" % "scala-reflect" % s"${scalaVersion.value}"
    case x if x.startsWith("2.") => deps :+ "org.scala-lang" % "scala-reflect" % s"${scalaVersion.value}"
    case _ => deps
  }
}

