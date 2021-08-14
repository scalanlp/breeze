scalacOptions in Global += "-deprecation"

resolvers += Resolver.url("artifactory", url("https://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(
  Resolver.ivyStylePatterns)

resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.typesafeRepo("releases")
)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")

//addSbtPlugin("com.github.sbt" % "sbt-jacoco" % "3.0.2")

addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "2.1.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.9.3")

addSbtPlugin("org.scalanlp" % "sbt-breeze-expand-codegen" % "0.2")
	
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.3")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.1")

addDependencyTreePlugin
