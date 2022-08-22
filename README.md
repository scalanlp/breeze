# Breeze [![Build Status](https://travis-ci.org/scalanlp/breeze.svg?branch=master)](https://travis-ci.org/scalanlp/breeze)

Breeze is a library for numerical processing. It aims to be generic, clean, and powerful without sacrificing (much) efficiency.

This is the 2.x branch. The 1.x branch is `1.x`.

The latest release is 2.1.0, which is cross-built against Scala 3.1, 2.12, and 2.13.

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
  "org.scalanlp" %% "breeze" % "2.1.0",
  
  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "2.1.0"
)


```

Previous versions of Breeze included a "breeze-natives" artifact that bundled various native libraries. As of Breeze 1.3, we now use a faster, more friendly-licensed library from @luhenry called simply "[netlib](https://github.com/luhenry/netlib)". This library is now bundled by default.


### Maven

Maven looks like this:

```xml
<dependency>
  <groupId>org.scalanlp</groupId>
  <artifactId>breeze_2.13</artifactId>
  <version>2.1.0</version>
</dependency>
```

### Other build tools
[http://mvnrepository.com/artifact/org.scalanlp/breeze_2.12/2.1.0] (as an example) is a great resource for finding other configuration examples for other build tools.

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

## Common Issues

### Segmentation Fault or Other Crashes on Linux

Netlib, the new low level BLAS library Breeze uses, in turn uses OpenBLAS by default on Linux, which has some quirky behavior w.r.t. threading. (Please see https://github.com/luhenry/netlib/issues/2).
As work arounds:

* Use MKL, if possible
* Increase the size of the stack of Java threads with `-Xss10M` (set the Java threads' stack size to 10 Mbytes)
* Make sure OpenBLAS doesn't use the parallel implementation by defining the environment variable `OPENBLAS_NUM_THREADS=1`
* Compile a custom version of OpenBLAS that unconditionally define `USE_ALLOC_HEAP` at https://github.com/xianyi/OpenBLAS/blob/develop/lapack/getrf/getrf_parallel.c#L49

