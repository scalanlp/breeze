scalacOptions in Global += "-deprecation"

//addSbtPlugin("com.github.sbt" % "sbt-jacoco" % "3.0.2")

addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "2.1.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.9.3")

addSbtPlugin("org.scalanlp" % "sbt-breeze-expand-codegen" % "0.2.1-SNAPSHOT")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.3")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.1")

 addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.5.5")
