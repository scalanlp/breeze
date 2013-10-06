# Breeze 

Breeze is a library for numerical processing. It aims to be generic, clean, and powerful without sacrificing (much) efficiency.

The current version is 0.5-SNAPSHOT. The latest release is 0.4. 

Note:  

Breeze used to have more components, but they are getting sharded out to different libraries. Their new homes are described below:

* breeze-viz: a (barely maintained) visualization library in its own repository. It is available at [https://github.com/scalanlp/breeze-viz](https://github.com/scalanlp/breeze-viz).
* breeze-learn (machine learning) is now in [Nak](https://github.com/scalanlp/nak).
* breeze-process (natural language processing) has become [Chalk](https://github.com/scalanlp/chalk).



## Build

This project can be built with sbt 0.12.3

### SBT

For **SBT**, Add these lines to your SBT project definition:

* For SBT versions 0.10.x or later

```scala
libraryDependencies  ++= Seq(
            // other dependencies here
            // pick and choose:
            "org.scalanlp" % "breeze_2.10" % "0.5-SNAPSHOT",
            // in a different repository now (github.com/scalanlp/breeze-viz)
            "org.scalanlp" % "breeze-viz_2.10" % "0.5-SNAPSHOT",
            // current releases: You can't use both.
            "org.scalanlp" % "breeze-math_2.10" % "0.4",
            "org.scalanlp" % "breeze-viz_2.10" % "0.4"
           
)

resolvers ++= Seq(
            // other resolvers here
            // if you want to use snapshot builds (currently 0.5-SNAPSHOT), use this.
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

// Scala 2.9.2 is still supported for 0.2.1, but is dropped afterwards.
scalaVersion := "2.10.3"
```

### Maven

Maven looks like this:

```xml
<dependency>
  <groupId>org.scalanlp</groupId>
	<artifactId>breeze-math_2.10</artifactId>
	<version>0.4-SNAPSHOT</version>
</dependency>
```

### Other build tools

http://mvnrepository.com/artifact/org.scalanlp/breeze-math_2.10/0.4 (as an example) is a great resource for finding other configuration examples for other build tools.

See documentation (linked below!) for more information on using Breeze.

## Documentation

* https://github.com/scalanlp/breeze/wiki/Quickstart
* https://github.com/scalanlp/breeze/wiki/Breeze-Linear-Algebra
* https://github.com/scalanlp/breeze/wiki/UserGuide
* [Scaladoc](http://www.scalanlp.org/api/#breeze.package)


## History

Breeze is the merger of the ScalaNLP and Scalala projects, because one of the original maintainers is unable to continue development. The Scalala parts are largely rewritten.

(c) David Hall, 2009 -

Portions (c) Daniel Ramage, 2009 - 2011

Contributions from:

* Jason Zaugg (@retronym)
* Alexander Lehmann (@afwlehmann)
* Jonathan Merritt (@lancelet)
* Keith Stevens (@fozziethebeat)
* Jason Baldridge (@jasonbaldridge)
* Timothy Hunter (@tjhunter)
* Dave DeCaprio (@DaveDeCaprio)
* Daniel Duckworth (@duckworthd)
* Eric Christiansen (@emchristiansen)
* Marc Millstone (@splittingfield)
* Mérő László (@laci37)
* Alexey Noskov (@alno)
* Devon Bryant (@devonbryant)
* Kentaroh Takagaki (@ktakagaki)

And others (contact David Hall if you've contributed code and aren't listed).

