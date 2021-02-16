Common.commonSettings

name := "breeze-macros"

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 10)) =>
      Seq(("org.scalamacros" %% "quasiquotes" % "2.1.0"),
        "org.scala-lang" % "scala-reflect" % s"${scalaVersion.value}"
      )
    case Some((2, _)) => Seq("org.scala-lang" % "scala-reflect" % s"${scalaVersion.value}")
    case Some((3, _)) => Seq()
  }
}
unmanagedSourceDirectories in Compile += {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => baseDirectory.value / "src" / "main" / "scala_2"
    case Some((3, _)) => baseDirectory.value / "src" / "main" / "scala_3"
  }
}
