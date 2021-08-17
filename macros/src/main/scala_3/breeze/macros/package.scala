package breeze

import breeze.macros.LoopMacros

import scala.language.dynamics
import scala.language.experimental.macros

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

  inline def assert(inline condition: Boolean, inline message: Any): Unit =
    ${ AssertImpl.assertImpl('condition, 'message)}

  transparent inline def assert(inline condition: Boolean): Unit =
    ${ AssertImpl.assertImpl('condition, null)}

  inline def require(inline condition: Boolean, inline message: Any): Unit =
    ${ AssertImpl.requireImpl('condition, 'message)}

  transparent inline def require(inline condition: Boolean): Unit =
    ${ AssertImpl.requireImpl('condition, null)}

  inline def assume(inline condition: Boolean, inline message: Any): Unit =
    ${ AssertImpl.assumeImpl('condition, 'message)}

  transparent inline def assume(inline condition: Boolean): Unit =
    ${ AssertImpl.assumeImpl('condition, null)}

}
