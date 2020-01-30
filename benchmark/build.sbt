Common.commonSettings

// lazy val breeze = project in file("core")
name := "breeze-benchmark"

libraryDependencies ++= Seq(
  "org.apfloat" % "apfloat" % "1.6.3",
  "org.jscience" % "jscience" % "4.3.1",
  "org.apache.commons" % "commons-math3" % "3.2",
  // thyme
  ("ichi.bench" % "thyme" % "0.1.0").from("http://plastic-idolatry.com/jars/thyme-0.1.0.jar"),
  // caliper stuff
  "com.google.guava" % "guava" % "r09",
  "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
  ("com.google.caliper" % "caliper" % "1.0-beta-2"),
  "com.google.code.gson" % "gson" % "1.7.1"
)

fork := true

publish := {}

publishLocal := {}

publishArtifact := false
