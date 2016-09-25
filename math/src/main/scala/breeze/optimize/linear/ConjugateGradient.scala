package breeze.optimize.linear

import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import breeze.math.MutableInnerProductVectorSpace
import breeze.util.Implicits._
import breeze.util.SerializableLogging

/**
 * Solve argmin (a dot x + .5 * x dot (B * x) + .5 * normSquaredPenalty * (x dot x)) for x
 * subject to norm(x) <= maxNormValue
 *
 * Based on the code from "Trust Region Newton Method for Large-Scale Logistic Regression"
 * * @author dlwh
 */
class ConjugateGradient[T,M](maxNormValue: Double = Double.PositiveInfinity,
                             maxIterations: Int = -1,
                             normSquaredPenalty: Double = 0,
                             tolerance: Double = 1E-5)
                            (implicit space: MutableInnerProductVectorSpace[T, Double], mult: OpMulMatrix.Impl2[M, T, T]) extends SerializableLogging {
  import space._

  def minimize(a: T, B: M): T = minimize(a, B, zeroLike(a))
  def minimize(a: T, B: M, initX: T): T = minimizeAndReturnResidual(a, B, initX)._1

  case class State private[ConjugateGradient](x: T, residual: T, private[ConjugateGradient] val direction: T, iter: Int, converged: Boolean)  {
    lazy val rtr = residual dot residual
  }

  /**
   * Returns the vector x and the vector r. x is the minimizer, while r is the residual error (which
   * may not be near zero because of the norm constraint.)
   * @param a
   * @param B
   * @param initX
   * @return
   */
  def minimizeAndReturnResidual(a: T, B: M, initX: T): (T,T) = {
    val state = iterations(a, B, initX).last
    state.x -> state.residual
  }

  def iterations(a: T, B: M, initX: T): Iterator[State] = Iterator.iterate(initialState(a, B, initX)) { state =>
    import state._
    var r = residual
    var d = direction
    var rtr = state.rtr
    val Bd = mult(B, d)
    val dtd = d dot d
    val alpha = math.pow(norm(r), 2.0) / ((d dot Bd) + normSquaredPenalty * dtd)
    val nextX = x + d * alpha

    val xnorm: Double = norm(nextX)
    if( xnorm >= maxNormValue) {// reached the edge. We're done.
      logger.info(f"$iter boundary reached! norm(x): $xnorm%.3f >= maxNormValue $maxNormValue")
      val xtd = x dot d
      val xtx = x dot x

      val normSquare = maxNormValue * maxNormValue

      val radius = math.sqrt(xtd * xtd + dtd * (normSquare - xtx))
      val alphaNext = if(xtd >= 0) {
        (normSquare - xtx) / (xtd + radius)
      } else {
        (radius - xtd) / dtd
      }

      assert(!alphaNext.isNaN, xtd +" " + normSquare + " " + xtx + "  " + xtd + " " + radius + " " +  dtd)
      axpy(alphaNext, d, x)
      axpy(-alphaNext, Bd + (d *:* normSquaredPenalty), r)

      State(x, r, d, iter + 1, converged = true)
    } else {
      x := nextX
      r -= (Bd + (d *:* normSquaredPenalty)) *:* alpha
      val newrtr = r dot r
      val beta = newrtr / rtr
      d :*= beta
      d += r
      rtr = newrtr
      val normr = norm(r)
      val converged = normr <= tolerance || (iter > maxIterations && maxIterations > 0)
      if(converged) {
        val done = iter > maxIterations && maxIterations > 0
        if(done)
          logger.info(f"max iteration $iter reached! norm(residual): $normr%.3f > tolerance $tolerance.")
        else
          logger.info(f"$iter converged! norm(residual): $normr%.3f <= tolerance $tolerance.")
      } else {
        logger.info(f"$iter: norm(residual): $normr%.3f > tolerance $tolerance.")
      }
      State(x, r, d, iter + 1, converged)
    }

  }.takeUpToWhere(_.converged)

  private def initialState(a: T, B: M, initX: T) = {
    val r = a - mult(B, initX) - (initX *:* normSquaredPenalty)
    val d = copy(r)
    val rnorm = norm(r)
    State(initX, r, d, 0, rnorm <= tolerance)
  }

}
