# Breeze [![Build Status](https://travis-ci.org/scalanlp/breeze.png?branch=master)](https://travis-ci.org/scalanlp/breeze)

Breeze is a library for numerical processing. It aims to be generic, clean, and powerful without sacrificing (much) efficiency.

The current version is 0.11.2. The latest release is 0.11.2.

## Documentation

* https://github.com/scalanlp/breeze/wiki/Quickstart
* https://github.com/scalanlp/breeze/wiki/Breeze-Linear-Algebra
* https://github.com/scalanlp/breeze/wiki/UserGuide
* [Scaladoc](http://www.scalanlp.org/api/breeze/)
* There is also the [scala-breeze google group](https://groups.google.com/forum/#!forum/scala-breeze) for general questions and discussion.

## Using Breeze

### Building it yourself.

This project can be built with sbt 0.13

### SBT

For **SBT**, Add these lines to your SBT project definition:

* For SBT versions 0.13.x or later

```scala
libraryDependencies  ++= Seq(
  // other dependencies here
  "org.scalanlp" %% "breeze" % "0.11.2",
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes. 
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.11.2",
  // the visualization library is distributed separately as well. 
  // It depends on LGPL code.
    "org.scalanlp" %% "breeze-viz" % "0.11.2"
)

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

// or 2.11.5
scalaVersion := "2.10.4"
```

For more details on the optional `breeze-natives` module, please watch Sam Halliday's talk at Scala eXchange 2014 [High Performance Linear Algebra in Scala](https://skillsmatter.com/skillscasts/5849-high-performance-linear-algebra-in-scala) ([follow along with high-res slides](http://fommil.github.io/scalax14/#/)).


### Maven

Maven looks like this:

```xml
<dependency>
  <groupId>org.scalanlp</groupId>
  <artifactId>breeze_2.10</artifactId> <!-- or 2.11 -->
  <version>0.10</version>
</dependency>
```

### Other build tools

http://mvnrepository.com/artifact/org.scalanlp/breeze_2.10/0.10 (as an example) is a great resource for finding other configuration examples for other build tools.

See documentation (linked above!) for more information on using Breeze.

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
* Sam Halliday (@fommil)
* Chris Stucchio (@stucchio)
* Xiangrui Meng (@mengxr)
* Gabriel Schubiner (@gabeos)
* Debasish Das (@debasish83)
* Julien Dumazert (@DumazertJulien)

Corporate Contributors:
* [Semantic Machines](http://www.semanticmachines.com/) (@semanticmachines)
* [ContentSquare](http://www.contentsquare.com/en/)
* Big Data Analytics,Verizon Lab, Palo Alto


And others (contact David Hall if you've contributed code and aren't listed).
