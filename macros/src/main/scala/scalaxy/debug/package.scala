// Originally part of Scalaxy. Modifications (c) 2015 David Hall, also under BSD
/*
 * SCALAXY LICENSE

Copyright (c) 2012-2015 Olivier Chafik, unless otherwise specified.
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

   3. Neither the name of Scalaxy nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 */

package scalaxy

import scala.language.dynamics
import scala.language.experimental.macros

import scala.reflect.ClassTag
import scala.reflect.NameTransformer.encode
import scala.reflect.macros.Context

package object debug
{
  def assert(condition: Boolean): Unit = macro impl.assertImpl

  def assert(condition: Boolean, message: String): Unit = macro impl.assertMsgImpl

  def require(condition: Boolean): Unit = macro impl.requireImpl

  def require(condition: Boolean, message: String): Unit = macro impl.requireMsgImpl

  def assume(condition: Boolean): Unit = macro impl.assumeImpl

  def assume(condition: Boolean, message: String): Unit = macro impl.assumeMsgImpl
}

package debug
{
object impl
{
  def assertImpl(c: Context)(condition: c.Expr[Boolean]): c.Expr[Unit] =
  {
    import c.universe._

    assertLikeImpl(c)(condition, (condExpr, messageExpr) => {
      reify(if (!condExpr.splice) throw new AssertionError("assertion failed: " + messageExpr.splice))
    })
  }

  def assertMsgImpl(c: Context)(condition: c.Expr[Boolean], message: c.Expr[String]): c.Expr[Unit] =
  {
    import c.universe._

    assertLikeImpl(c)(condition, (condExpr, messageExpr) => {
      reify(if (!condExpr.splice) throw new AssertionError("assertion failed: " + message.splice + ": " + messageExpr.splice))
    })
  }

  def requireImpl(c: Context)(condition: c.Expr[Boolean]): c.Expr[Unit] =
  {
    import c.universe._

    assertLikeImpl(c)(condition, (condExpr, messageExpr) => {
      reify(if (!condExpr.splice) throw new IllegalArgumentException("requirement failed: "+ messageExpr.splice))
    })
  }

  def requireMsgImpl(c: Context)(condition: c.Expr[Boolean], message: c.Expr[String]): c.Expr[Unit] =
  {
    import c.universe._

    assertLikeImpl(c)(condition, (condExpr, messageExpr) => {
      reify(if (!condExpr.splice) throw new IllegalArgumentException("requirement failed: "+ message.splice + ": " + messageExpr.splice))
    })
  }


  def assumeImpl(c: Context)(condition: c.Expr[Boolean]): c.Expr[Unit] =
  {
    import c.universe._

    assertLikeImpl(c)(condition, (condExpr, messageExpr) => {
      reify(if (!condExpr.splice) throw new AssertionError("assumption failed: " + messageExpr.splice))
    })
  }

  def assumeMsgImpl(c: Context)(condition: c.Expr[Boolean], message: c.Expr[String]): c.Expr[Unit] =
  {
    import c.universe._

    assertLikeImpl(c)(condition, (condExpr, messageExpr) => {
      reify(if (!condExpr.splice) throw new AssertionError("assumption failed: " + message.splice + ": " + messageExpr.splice))
    })
  }

  def assertLikeImpl
  (c: Context)
  (
    condition: c.Expr[Boolean],
    callBuilder: (c.Expr[Boolean], c.Expr[String]) => c.Expr[Unit]
    ): c.Expr[Unit] =
  {
    import c.universe._

    def newValDef(name: String, rhs: Tree, tpe: Type = null) = {
      ValDef(
        NoMods,
        newTermName(c.fresh(name)),
        TypeTree(Option(tpe).getOrElse(rhs.tpe.normalize)),
        rhs
      )
    }

    object EqualityOpName {
      def unapply(name: Name): Option[Boolean] = {
        val s = name.toString
        if (s == encode("==")) Some(true)
        else if (s == encode("!=")) Some(false)
        else None
      }
    }

    def isConstant(tree: Tree) = tree match {
      case Literal(Constant(_)) => true
      case _ => false
    }

    val typedCondition = c.typeCheck(condition.tree)//, typeOf[Boolean])
    c.Expr[Unit](
      typedCondition match
      {
        case Apply(Select(left, op @ EqualityOpName(isEqual)), List(right)) =>
          val leftDef = newValDef("left", left)
          val rightDef = newValDef("right", right)

          val leftExpr = c.Expr[Any](Ident(leftDef.name))
          val rightExpr = c.Expr[Any](Ident(rightDef.name))

          val rels = ("==", "!=")
          val (expectedRel, actualRel) = if (isEqual) rels else rels.swap
          val actualRelExpr = q"$actualRel"
          val str = c.literal(s"$left $expectedRel $right")
          val condExpr = c.Expr[Boolean](
            Apply(Select(Ident(leftDef.name), op), List(Ident(rightDef.name)))
          )
          Block(
            leftDef,
            rightDef,
            if (isEqual)
              callBuilder(
                condExpr,
                c.Expr(q""" ${""} + $str + " (" + $leftExpr + " " + $actualRelExpr + " " + $rightExpr + ")" """)
                // c.Expr[String](q""" "%s (%s %s %s)".format($str, $leftExpr, $actualRelExpr, $rightExpr)""")
                // reify(s"${str.splice} (${leftExpr.splice} ${actualRelExpr.splice} ${rightExpr.splice})")
              ).tree
            else if (isConstant(left) || isConstant(right))
              callBuilder(condExpr, str).tree
            else
              callBuilder(condExpr, reify(s"${str.splice} (== ${leftExpr.splice})")).tree
          )
        case _ =>
          val condDef = newValDef("cond", typedCondition)
          val condExpr = c.Expr[Boolean](Ident(condDef.name))
          val str = c.Expr[String](
            if (isConstant(typedCondition))
              q""" "Always false!" """
            else
              q"${typedCondition.toString}"
          )
          Block(
            condDef,
            callBuilder(condExpr, str).tree
          )
      }
    )
  }
}
}
