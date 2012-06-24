addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.7.2")

libraryDependencies ++= Seq(
  "org.jacoco" % "org.jacoco.core" % "0.5.6.201201232323" artifacts(Artifact("org.jacoco.core", "jar", "jar")),
  "org.jacoco" % "org.jacoco.report" % "0.5.6.201201232323" artifacts(Artifact("org.jacoco.report", "jar", "jar")))

addSbtPlugin("de.johoop" % "jacoco4sbt" % "1.2.2")

scalacOptions in Global += "-deprecation"
