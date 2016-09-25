Common.commonSettings

name := "breeze-macros"



libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case x if x.startsWith("2.10") =>
      deps :+ ("org.scalamacros" %% "quasiquotes" % "2.1.0")
    case _ => deps
  }
}

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

