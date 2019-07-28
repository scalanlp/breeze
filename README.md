# Breeze [![Build Status](https://travis-ci.org/scalanlp/breeze.svg?branch=master)](https://travis-ci.org/scalanlp/breeze)

Breeze is a library for numerical processing. It aims to be generic, clean, and powerful without sacrificing (much) efficiency.

The latest release is 1.0, which is cross-built against Scala 2.11, 2.12, and 2.13.

## Documentation

* https://github.com/scalanlp/breeze/wiki/Quickstart
* https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet
* [Scaladoc](http://www.scalanlp.org/api/breeze/) (Scaladoc is typically horribly out of date, and not a good way to learn Breeze.)
* There is also the [scala-breeze google group](https://groups.google.com/forum/#!forum/scala-breeze) for general questions and discussion.


## Using Breeze

### Building it yourself

This project can be built with SBT 1.2+

### SBT

For SBT, add these lines to your SBT project definition:

```scala
libraryDependencies  ++= Seq(
  // Last stable release
  "org.scalanlp" %% "breeze" % "1.0",
  
  // Native libraries are not included by default. add this if you want them
  // Native libraries greatly improve performance, but increase jar sizes. 
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "1.0",
  
  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "1.0"
)


```

For more details on the optional `breeze-natives` module, please watch Sam Halliday's talk at Scala eXchange 2014 [High Performance Linear Algebra in Scala](https://skillsmatter.com/skillscasts/5849-high-performance-linear-algebra-in-scala) ([follow along with high-res slides](http://fommil.github.io/scalax14/#/)).


### Maven

Maven looks like this:

```xml
<dependency>
  <groupId>org.scalanlp</groupId>
  <artifactId>breeze_2.13</artifactId>
  <version>1.0</version>
</dependency>
```

### Other build tools
[http://mvnrepository.com/artifact/org.scalanlp/breeze_2.12/1.0] (as an example) is a great resource for finding other configuration examples for other build tools.

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
* Matthias Langer (@bashimao)
* Mohamed Kafsi (@mou7)
* Max Thomas (@maxthomas)
* @qilab
* Weichen Xu (@WeichenXu123)
* Sergei Lebedev (@superbobry)
* Zac Blanco (@ZacBlanco)

Corporate (Code) Contributors:
* [Semantic Machines](http://www.semanticmachines.com/) (@semanticmachines)
* [ContentSquare](http://www.contentsquare.com/en/)
* Big Data Analytics, Verizon Lab, Palo Alto
* [crealytics GmbH, Berlin/Passau, Germany](https://crealytics.com/)


And others (contact David Hall if you've contributed and aren't listed).
