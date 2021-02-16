package breeze

import breeze.macros.LoopMacros

package object macros {
  inline def cforRange(inline range: Range)(inline body: Int=>Any): Unit = {
    ${LoopMacros.cforRangeImpl('range, 'body)}
  }
}
