package breeze.optimize.linear

import breeze.math.{MutableInnerProductSpace, TensorSpace}
import breeze.linalg.operators.{OpMulMatrix, BinaryOp}

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
                            (implicit space: MutableInnerProductSpace[T, Double], mult: BinaryOp[M, T, OpMulMatrix, T]) {
  import space._

  def minimize(a: T, B: M): T = minimizeAndReturnResidual(a, B, zeros(a))._1
  def minimize(a: T, B: M, initX: T): T = minimizeAndReturnResidual(a, B, initX)._1

  /**
   * Returns the vector x and the vector r. x is the minimizer, while r is the residual error (which
   * may not be near zero because of the norm constraint.)
   * @param a
   * @param B
   * @param initX
   * @return
   */
  def minimizeAndReturnResidual(a: T, B: M, initX: T): (T,T) = {

    var r = a - mult(B, initX) - initX * normSquaredPenalty
    var d = copy(r)
    var x = copy(initX)

    var rtr = r dot r
    var iter = 0


    var converged = norm(r) <= tolerance
    while(!converged) {
      val Bd = mult(B, d)
      val dtd = d dot d
      val alpha = math.pow(norm(r), 2.0) / (d.dot(Bd) + normSquaredPenalty * (dtd))
      val nextX = x + d * alpha

      if( norm(nextX) >= maxNormValue) {
        val xtd = x dot d
        val xtx = x dot x

        val normSquare = maxNormValue * maxNormValue

        val radius = math.sqrt(xtd * xtd + dtd * (normSquare - xtx))
        val alphaNext = if(xtd >= 0) {
          (normSquare - xtx) / (xtd + radius)
        } else {
          (radius - xtd) / (dtd)
        }

        assert(!alphaNext.isNaN, xtd +" " + normSquare + " " + xtx + "  " + xtd + " " + radius + " " +  dtd)
        x += d * alphaNext
        r -= (Bd + d * normSquaredPenalty) * alphaNext


        converged = true

      } else {
        x := nextX
        r -= (Bd + d * normSquaredPenalty) * alpha
        val newrtr = r dot r
        val beta = newrtr / rtr
        d *= beta
        d += r
        rtr = newrtr
        converged = norm(r) <= tolerance || (iter > maxIterations && maxIterations > 0)
        iter += 1

      }
    }

    println(iter)
    x -> r


  }

}
