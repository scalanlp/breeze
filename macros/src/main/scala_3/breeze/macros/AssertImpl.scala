package breeze.macros

import scala.quoted._

object AssertImpl {
  // Null to deal with macro limitations
  def assertImpl(condition: Expr[Boolean], message: Expr[Any]|Null)(using Quotes) = {
    assertLikeImpl(condition, message, assertBody)
  }
  
  def assertBody(using Quotes): Expr[String] => Expr[Nothing] = m => '{
    throw new AssertionError(f"assertion failed: ${$m}")
  }

  def requireImpl(condition: Expr[Boolean], message: Expr[Any]|Null)(using Quotes) = {
    assertLikeImpl(condition, message, requireBody)
  }

  def requireBody(using Quotes): Expr[String] => Expr[Nothing] = m => '{
    throw new IllegalArgumentException(f"requirement failed: ${$m}")
  }

  def assumeImpl(condition: Expr[Boolean], message: Expr[Any]|Null)(using Quotes) = {
    assertLikeImpl(condition, message, assumeBody)
  }

  def assumeBody(using Quotes): Expr[String] => Expr[Nothing] = m => '{
    throw new IllegalArgumentException(f"assumption failed: ${$m}")
  }

  def assertLikeImpl(condition: Expr[Boolean], message: Expr[Any]|Null, doThrow: Expr[String]=>Expr[Nothing])(using Quotes) = {
    val condStr = Expr(condition.show)
    Option(message) match {
      case None => '{
        if !$condition then ${doThrow(condStr)}
      }
      case Some(m) => 
        '{
        if !$condition then ${doThrow('{ s"${$m}: ${$condStr}"})}
      }
    }
  }

  //  def showExpr[T](expr: Expr[T])(using Quotes): Expr[String] =
//    val code: String = expr.show
//    Expr(code)


}
