package breeze.optimize

import scala.actors._
import breeze.linalg.DenseVector
import breeze.math.InnerProductSpace
import com.typesafe.scalalogging.log4j.Logging


class SPG(
  val optTol: Double = 1e-4, // termination criterion: tolerance for norm of projected gradient
  val gamma: Double = 1e-4, // sufficient decrease parameter
  val M: Int = 10, // number of history entries for linesearch
  val alphaMax: Double = 1e10, // longest step
  val alphaMin: Double = 1e-10, // shortest step
  val maxNumIt: Int = 1000, // maximum number of iterations
  val testOpt: Boolean = true, // perform optimality check based on projected gradient at each iteration
  val initFeas: Boolean = false, // is the initial guess feasible, or should it be projected?
  val maxSrchIt: Int = 30, // maximum number of line search attempts
  val projection: DenseVector[Double] => DenseVector[Double] = identity) extends Minimizer[DenseVector[Double], DiffFunction[DenseVector[Double]]] with Logging {

  override def minimize(prob: DiffFunction[DenseVector[Double]], guess: DenseVector[Double]): DenseVector[Double] = {
    def correctedGradient(x: DenseVector[Double], g: DenseVector[Double]): DenseVector[Double] = projection(x - g) - x
    var gnorm: Double = 0.0
    var x = if (initFeas) guess.copy else projection(guess.copy)

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

      gnorm = correctedGradient(x, g).norm(Double.PositiveInfinity)
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

    return x
  }

  def computeStep(newx: DenseVector[Double], oldx: DenseVector[Double],
    newg: DenseVector[Double], oldg: DenseVector[Double]): Double = {
    val s = newx - oldx
    val y = newg - oldg
    return s.dot(s) / s.dot(y)
  }

}
