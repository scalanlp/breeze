name := "breeze-viz"

Common.commonSettings

libraryDependencies ++= Seq(
  "org.jfree" % "jfreechart" % "1.5.3",
  "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.3.1", // for eps gen
  // "org.apache.xmlgraphics" % "batik-dom" % "1.7",    // for svg gen
  // "org.apache.xmlgraphics" % "batik-svggen" % "1.7", // for svg gen
  ("com.lowagie" % "itext" % "2.1.5").intransitive() // for pdf gen
)

javaOptions += "-Xmx2g"
