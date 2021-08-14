Common.commonSettings

name := "breeze-macros"

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => 
      Seq(
        "org.scala-lang" % "scala-reflect" % s"${scalaVersion.value}",
        ("org.typelevel" %% "spire" % "0.17.0").cross(CrossVersion.for3Use2_13)
      )
    case Some((3, _)) => Seq(
    )
    case _ => ???
  }
}
unmanagedSourceDirectories in Compile += {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => baseDirectory.value / "src" / "main" / "scala_2"
    case Some((3, _)) => baseDirectory.value / "src" / "main" / "scala_3"
    case _ => ???
  }
}

unmanagedSourceDirectories in Test += {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => baseDirectory.value / "src" / "test" / "scala_2"
    case Some((3, _)) => baseDirectory.value / "src" / "test" / "scala_3"
    case _ => ???
  }
}
