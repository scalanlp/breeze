## Notes on Code Generation

TL;DR: Add math/src/main/codegen as a sources root in intellij.

NOTE: If someone knows how to make that happen automatically I'd appreciate it.

Breeze makes fairly extensive use of code generation to achieve a lot of its performance. 
In particular, we use the `@expand` annotation to generate optimized implementations for various languages
numeric and linear algebra operations. Until April 2021, `@expand` was implemented via a macro annotation.
However, Scala 3 will not support macro annotations, and I (@dlwh) don't particularly want to support two versions
of the codebase. Nor do I want to check in a whole bunch of generated code for when we inevitably change things later.

So I'm using [Scala Meta](https://github.com/scalameta/scalameta) instead to do explicit source generation.
Scala Meta was relatively easy to drop in for macro annotations modulo some annoying differences, though there were a bunch of issues. I think it
may have been wiser to switch to Scalafix.

The pre-generated files live in `math/src/main/codegen`. I wrote a janky SBT plugin that 
contains the ported unrolling logic at https://github.com/dlwh/sbt-breeze-expand-codegen.
