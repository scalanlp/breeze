package breeze

import spire.syntax.cfor._

package object macros {
  def cforRange(r: Range)(body: Int => Unit): Unit =
  macro Syntax.cforRangeMacro

}
