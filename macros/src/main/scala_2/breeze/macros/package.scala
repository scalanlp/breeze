package breeze

import spire.macros.Syntax

package object macros {
  def cforRange(r: Range)(body: Int => Unit): Unit = macro Syntax.cforRangeMacro
  def cforRange2(r1: Range, r2: Range)(body: (Int, Int) => Unit): Unit = macro Syntax.cforRange2Macro
}
