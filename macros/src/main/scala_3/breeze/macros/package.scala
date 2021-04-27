package breeze

import breeze.macros.LoopMacros

package object macros {
  inline def cforRange(inline range: Range)(inline body: Int=>Any): Unit = {
    ${LoopMacros.cforRangeImpl('range, 'body)}
  }

  inline def cforRange2(inline range1: Range, inline range2: Range)(inline body: (Int, Int)=>Any): Unit = {
    cforRange(range1)(i => cforRange(range2)(j => body(i, j)) )
  }

//  inline def cforRange[T](inline range: NumericRange[T])(inline body: T=>Any): Unit = {
//    ${LoopMacros.cforRangeImpl('range, 'body)}
//  }
}
