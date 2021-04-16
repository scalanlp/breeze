package breeze.macros

import scala.collection.immutable.NumericRange
import scala.quoted._
import scala.compiletime.constValue

object LoopMacros {
  def cforRangeImpl(range: Expr[Range], body: Expr[Int=>Any])(using quotes: Quotes): Expr[Unit] = range match {
    case '{ ($x: Int) until ($y: Int)} => loopBody(true, x, y, Expr(1), body)
    case '{ ($x: Int) until ($y: Int) by ($step: Int)} => loopBody(true, x, y, step, body)
    case '{ ($x: Int) to ($y: Int)} => loopBody(false, x, y, Expr(1), body)
    case '{ ($x: Int) to ($y: Int) by ($step: Int)} => loopBody(false, x, y, step, body)
    case _ =>
      // TODO: there are more cases we can manage...
      quotes.reflect.report.warning("Nonliteral range, so can't simplify", range)
      '{ $range.foreach($body) }
  }

  private def loopBody(exclusive: Boolean, begin: Expr[Int], end: Expr[Int], step: Expr[Int], body: Expr[Int=>Any])(using Quotes) = '{
    var i: Int = $begin
    val yy = $end
    while ${loopCondition('i, exclusive, end, step)} do
      ${Expr.betaReduce( '{ $body(i)})}
      i += $step
  }
  
  private def loopCondition(loopVar: Expr[Int], exclusive: Boolean, end: Expr[Int], stride: Expr[Int])(using q: Quotes): Expr[Boolean] = {
    stride.value match {
      case Some(x) if x > 0 => 
        if exclusive then '{ $loopVar < $end}
        else '{ $loopVar <= $end }
      case Some(x) if x < 0 =>
        if exclusive then '{ $loopVar > $end}
        else '{ $loopVar >= $end }
      case Some(0) => 
        q.reflect.report.error("stride shouldn't be zero, but got " + stride.show)
        if exclusive then '{ $loopVar < $end}
        else '{ $loopVar <= $end }
      case _ =>
        if exclusive then '{ ($stride > 0 && $loopVar < $end) || ($stride < 0 && $loopVar > $end) || ($stride == 0)}
        else '{ ($stride > 0 && $loopVar <= $end) || ($stride < 0 && $loopVar >= $end) || ($stride == 0)}
    }
  }
  

}

