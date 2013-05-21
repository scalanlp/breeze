package breeze.optimize

import breeze.math.MutableCoordinateSpace
import com.typesafe.scalalogging.log4j.Logging
import breeze.collection.mutable.RingBuffer


/**
 *
 * @tparam T vector type
 * @param optTol termination criterion: tolerance for norm of projected gradient
 * @param gamma  sufficient decrease parameter
 * @param M number of history entries for linesearch
 * @param alphaMax longest step
 * @param alphaMin shortest step
 * @param maxNumIt maximum number of iterations
 * @param testOpt perform optimality check based on projected gradient at each iteration
 * @param initFeas is the initial guess feasible, or should it be projected?
 * @param maxSrchIt maximum number of line search attempts
 * @param projection projection operations
 */
class SPG[T](
 val projection: T => T = {(t:T) =>t},
 val optTol: Double = 1e-4,
  val gamma: Double = 1e-4,
  val M: Int = 10,
  val alphaMax: Double = 1e10,
  val alphaMin: Double = 1e-10,
  val maxNumIt: Int = 1000,
  val testOpt: Boolean = true,
  val initFeas: Boolean = false,
  val maxSrchIt: Int = 30)(implicit coord: MutableCoordinateSpace[T, Double]) extends Minimizer[T, DiffFunction[T]] with Logging {
  import coord._

  override def minimize(prob: DiffFunction[T], guess: T): T = {
    def correctedGradient(x: T, g: T): T = projection(x - g) - x
    var gnorm: Double = 0.0
    var x = if (initFeas) copy(guess) else projection(copy(guess))

    var alpha = 1.0 //0.001 / gnorm
    var prevfs = new RingBuffer[Double](M)
    var t = 1
    var fevals = 1

    do {
      var g = prob.gradientAt(x)

      val searchDirection = correctedGradient(x, g * alpha)
      val gTd = searchDirection.dot(g)

      prevfs += prob.valueAt(x)
      var lambda = 1.0
      var accepted = false
      var srchit = 0

      gnorm = norm(correctedGradient(x, g))
      // Backtracking line-search
      do {
        val candx = x + searchDirection * lambda
        val candg = prob.gradientAt(candx)
        val candf = prob.valueAt(candx)
        val suffdec = gamma * lambda * gTd

        if (prevfs.exists(candf <= _ + suffdec)) {
          alpha = alphaMax.min(alphaMin.max(computeStep(candx, x, candg, g)))
          accepted = true
          g = candg
          x = candx
        } else if (srchit >= maxSrchIt) {
          accepted = true
        } else {
          lambda *= 0.3
          srchit = srchit + 1
        }
        fevals = fevals + 1
      } while (!accepted)

      if (srchit >= maxSrchIt) {
        return x
      }

      t = t + 1
    } while (((testOpt == false) || (gnorm > optTol))
      && (t < maxNumIt) //  && (!prob.hasConverged)
      )

    x
  }

  def computeStep(newx: T, oldx: T, newg: T, oldg: T): Double = {
    val s = newx - oldx
    val y = newg - oldg
    s.dot(s) / s.dot(y)
  }

}
