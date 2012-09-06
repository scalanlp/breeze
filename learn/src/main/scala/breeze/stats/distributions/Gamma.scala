package breeze.stats
package distributions

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

import breeze.numerics._
import breeze.optimize.{LBFGS, DiffFunction}
import breeze.linalg.DenseVector
import math.pow


/**
 * Represents a Gamma distribution.
 * E[X] = shape * scale
 *
 * @author dlwh
 */
case class Gamma(val shape : Double, val scale : Double)(implicit rand: RandBasis = Rand)
    extends ContinuousDistr[Double] with Moments[Double] {
  if(shape <= 0.0 || scale <= 0.0)
    throw new IllegalArgumentException("Shape and scale must be positive")

  val logNormalizer = lgamma(shape) + shape * log(scale)

  override def unnormalizedLogPdf(x : Double) = (shape - 1) * log(x) - x/scale

  override def toString = "Gamma(" + shape + "," + scale + ")"

  // Copied from Teh
  /*
  def draw() : Double = { 
    var aa = 0.0
    var bb = 0.0
    var cc = 0.0
    var dd = 0.0
    var uu = 0.0
    var vv = 0.0
    var ww = 0.0
    var xx = 0.0
    var yy = 0.0
    var zz = 0.0
    if (shape == 1.0) {
      /* Exponential */
      scale * -math.log(rand.uniform.get)
    } else if (shape < 1.0) {
      /* Use Johnks generator */
      cc = 1.0 / shape
      dd = 1.0 / (1.0 - shape)
      while (true) {
        xx = pow(rand.uniform.get(), cc)
        yy = xx + pow(rand.uniform.get(), dd)
        if (yy <= 1.0) {
          return scale * -log(rand.uniform.get()) * xx / yy
        }
      }
      sys.error("shouldn't get here")
    } else { /* shape > 1.0 */
      /* Use bests algorithm */
      bb = shape - 1.0
      cc = 3.0 * shape - 0.75
      while (true) {
        uu = rand.uniform.get()
        vv = rand.uniform.get()
        ww = uu * (1.0 - uu)
        yy = math.sqrt(cc / ww) * (uu - 0.5)
        xx = bb + yy
        if (xx >= 0) {
          zz = 64.0 * ww * ww * ww * vv * vv
          if ((zz <= (1.0 - 2.0 * yy * yy / xx))
              || (math.log(zz) <= 2.0 * (bb * math.log(xx / bb) - yy))) {
            return xx * scale
          }
        }
      }
      sys.error("shouldn't get here")
    }
  }
  */

  def draw() = {
    if(shape == 1.0) {
      scale * -math.log(rand.uniform.draw())
    } else if (shape < 1.0) {
      val c = 1.0 + shape/scala.math.E
      var d = c * rand.uniform.draw()
      var ok = false
      var x = 0.0
      while(!ok) {
        if (d >= 1.0) {
          x = -log((c - d) / shape)
          if (-math.log(rand.uniform.draw()) >= (1.0 - shape) * log(x)) {
            x = (scale * x)
            ok = true
          }
        } else {
          x = math.pow(d, 1.0/shape)
          if (-math.log(rand.uniform.draw()) >= (1.0 - shape) * log(x)) {
            x = scale * x
            ok = true
          }
        }
        d = c * rand.uniform.draw()
      }
      x
    } else {
      // from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
      val d = shape-1.0/3.0
      val c = 1.0 / math.sqrt(9.0* d)
      var r = 0.0
      var ok = false
      while (!ok) {
        var v = 0.0
        var x = 0.0
        do {
          x = rand.gaussian(0, 1).draw()
          v = 1.0 + c * x
        } while(v <= 0)

        v = v*v*v
        val x2 = x * x
        val u = rand.uniform.draw()
        if (  u < 1.0 - 0.0331 * (x2 * x2)
          || log(u) < 0.5*x2 + d* (1.0 - v+log(v))) {
          r = (scale*d*v)
          ok = true
        }
      }
      r
    }
  }

  def mean = shape * scale
  def variance = mean * scale
  def mode = { require(shape >= 1); mean - scale}
  def entropy = logNormalizer - (shape - 1) * digamma(shape) + shape
}

object Gamma extends ExponentialFamily[Gamma,Double] {
  type Parameter = (Double,Double)
  import breeze.stats.distributions.{SufficientStatistic=>BaseSuffStat}
  case class SufficientStatistic(n: Double, meanOfLogs: Double, mean: Double) extends BaseSuffStat[SufficientStatistic] {
    def *(weight: Double) = SufficientStatistic(n*weight, meanOfLogs*weight, mean * weight)
    def +(t: SufficientStatistic) = {
      val delta = t.mean - mean
      val newMean = mean + delta * (t.n /(t.n + n))
      val logDelta = t.meanOfLogs - meanOfLogs
      val newMeanLogs = meanOfLogs + logDelta * (t.n /(t.n + n))
      SufficientStatistic(t.n+n, newMeanLogs, newMean)
    }
  }

  def emptySufficientStatistic = SufficientStatistic(0,0,0)

  def sufficientStatisticFor(t: Double) = SufficientStatistic(1,math.log(t),t)

  // change from Timothy Hunter. Thanks!
  def mle(ss: SufficientStatistic) = {
    val s = math.log( ss.mean ) - ss.meanOfLogs
    assert(s > 0 , s) // check concavity
    val k_approx = approx_k(s)
    assert(k_approx > 0 , k_approx)
    val k = Nwt_Rph_iter_for_k(k_approx, s)
    val theta = ss.mean / (k)
    (k, theta)
  }
  /*
   * s = log( x_hat) - log_x_hat
   */
  def approx_k(s:Double) : Double = {
    // correct within 1.5%
    (3 - s + math.sqrt( math.pow((s-3),2) + 24*s )) / (12 * s)
  }

  def Nwt_Rph_iter_for_k(k:Double, s:Double ): Double = {
    /*
     * For a more precise estimate, use Newton-Raphson updates
     */
    val k_new = k - (math.log(k) - digamma(k) - s)/( 1.0/k - trigamma(k) )
    if (math.abs(k - k_new)/math.abs(k_new) < 1.0e-4)
      k_new
    else
      Nwt_Rph_iter_for_k(k_new,s)
  }


  def distribution(p: Parameter) = new Gamma(p._1,p._2)

  def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[(Double,Double)] {
    val SufficientStatistic(n,meanOfLogs,mean) = stats
    def calculate(x: (Double,Double)) = {
      val (a,b) = x
      val obj = -n * ((a - 1) * meanOfLogs - lgamma(a) - a * log(b)- mean / b)
      val gradA = -n * (meanOfLogs - digamma(a)  - log(b))
      val gradB = -n * (-a/b + mean / b / b)
      (obj,(gradA,gradB))
    }
  }
}
