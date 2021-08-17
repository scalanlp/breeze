package breeze

import spire.macros.Syntax

package object macros {
  def cforRange(r: Range)(body: Int => Unit): Unit = macro Syntax.cforRangeMacro
  def cforRange2(r1: Range, r2: Range)(body: (Int, Int) => Unit): Unit = macro Syntax.cforRange2Macro

  def assert(condition: Boolean): Unit = macro AssertImpl.assertImpl

  def assert(condition: Boolean, message: String): Unit = macro AssertImpl.assertMsgImpl

  def require(condition: Boolean): Unit = macro AssertImpl.requireImpl

  def require(condition: Boolean, message: String): Unit = macro AssertImpl.requireMsgImpl

  def assume(condition: Boolean): Unit = macro AssertImpl.assumeImpl

  def assume(condition: Boolean, message: String): Unit = macro AssertImpl.assumeMsgImpl
}
