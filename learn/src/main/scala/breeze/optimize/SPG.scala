package breeze.optimize

import scala.actors._
import breeze.linalg.DenseVector
import breeze.math.InnerProductSpace
import com.typesafe.scalalogging.log4j.Logging

object ProjectableProblem {
  def withL2Regularization(d: DiffFunction[DenseVector[Double]], weight: Double)(implicit vspace: InnerProductSpace[DenseVector[Double], Double]) = new ProjectableProblem {
    import vspace._
    override def gradientAt(x: DenseVector[Double]): DenseVector[Double] = {
      val grad = d.gradientAt(x)
      myGrad(grad, x)
    }

    override def valueAt(x: DenseVector[Double]) = {
      val v = d.valueAt(x)
      myValueAt(v, x)
    }

    private def myValueAt(v: Double, x: DenseVector[Double]) = {
      v + weight * (x dot x) / 2
    }

    private def myGrad(g: DenseVector[Double], x: DenseVector[Double]): DenseVector[Double] = {
      g + (x * weight)
    }

    override def calculate(x: DenseVector[Double]) = {
      val (v, grad) = d.calculate(x)
      (myValueAt(v, x), myGrad(grad, x))
    }
    override def project(x: DenseVector[Double]) = x
  }
}

abstract class ProjectableProblem extends DiffFunction[DenseVector[Double]] {
  def project(x: DenseVector[Double]): DenseVector[Double] = x
}

class SPG(
  val optTol: Double = 1e-4, // termination criterion: tolerance for norm of projected gradient
  val gamma: Double = 1e-4, // sufficient decrease parameter
  val M: Int = 10, // number of history entries for linesearch
  val alphaMax: Double = 1e10, // longest step
  val alphaMin: Double = 1e-10, // shortest step
  val maxNumIt: Int = 1000, // maximum number of iterations
  val testOpt: Boolean = true, // perform optimality check based on projected gradient at each iteration
  val initFeas: Boolean = false, // is the initial guess feasible, or should it be projected?
  val maxSrchIt: Int = 30 // maximum number of line search attempts
  ) extends Minimizer[DenseVector[Double], ProjectableProblem] with  Logging {

  override def minimize(prob: ProjectableProblem, guess: DenseVector[Double]): DenseVector[Double] = {
    def computeGradient(x: DenseVector[Double], g: DenseVector[Double]): DenseVector[Double] = prob.project(x - g) - x

    def computeGradientNorm(x: DenseVector[Double], g: DenseVector[Double]): (Double, DenseVector[Double]) = {
      val w = computeGradient(x, g)
      return (w.norm(Double.PositiveInfinity), w)
    }
    var x = if (initFeas) guess.copy else prob.project(guess.copy)
    var g = prob.gradientAt(x)
    var f = prob.valueAt(x)

    var candx: DenseVector[Double] = computeGradient(x, g)
    var gnorm = candx.norm(Double.PositiveInfinity)
    var alpha = 1.0 //0.001 / gnorm
    var prevfs = new RingBuffer[Double](M)
    var t = 1
    var fevals = 1
    var gTd = 0.0
    var d = DenseVector.zeros[Double](0)

    while (((testOpt == false) || (gnorm > optTol))
      && (t < maxNumIt) //  && (!prob.hasConverged)
      ) {
      d = computeGradient(x, g * alpha)
      gTd = d.dot(g)
      if (testOpt) {
        candx = computeGradient(x, g)
        gnorm = candx.norm(Double.PositiveInfinity)
      }

      prevfs += f
      var lambda = 1.0
      var accepted = false
      var srchit = 0

      // Backtracking line-search
      do {
        candx = x + d * lambda
        val candg = prob.gradientAt(candx)
        val candf = prob.valueAt(candx)
        val suffdec = gamma * lambda * gTd

        if (prevfs.exists(candf <= _ + suffdec)) {
          alpha = alphaMax.min(alphaMin.max(computeStep(candx, x, candg, g)))
          f = candf
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
    }

    return x
  }

  def computeStep(newx: DenseVector[Double], oldx: DenseVector[Double],
    newg: DenseVector[Double], oldg: DenseVector[Double]): Double = {
    val s = newx - oldx
    val y = newg - oldg
    return s.dot(s) / s.dot(y)
  }

}
