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
import breeze.optimize.DiffFunction
import org.apache.commons.math3.distribution.GammaDistribution

import scala.annotation.tailrec
import scala.math.pow

/**
 * Represents a Gamma distribution.
 * E[X] = shape * scale
 *
 * @author dlwh
 */
case class Gamma(shape: Double, scale: Double)(implicit rand: RandBasis = Rand)
    extends ContinuousDistr[Double]
    with Moments[Double, Double]
    with HasCdf
    with HasInverseCdf {
  if (shape <= 0.0 || scale <= 0.0)
    throw new IllegalArgumentException("Shape and scale must be positive")

  override def pdf(x: Double): Double = {
    if (x < 0) {
      0.0
    } else if (x > 0) {
      math.exp(logPdf(x))
    } else {
      if (shape > 1.0) {
        0.0
      } else if (shape == 1.0) {
        normalizer
      } else {
        Double.PositiveInfinity
      }
    }
  }

  lazy val logNormalizer: Double = lgamma(shape) + shape * log(scale)

  override def unnormalizedLogPdf(x: Double) = (shape - 1) * log(x) - x / scale

  override def toString = "Gamma(" + shape + "," + scale + ")"

  def logDraw() =
    if (shape < 1) {
      // adapted from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
      @tailrec
      def rec: Double = {
        val u = rand.uniform.draw()
        val v = -math.log(rand.uniform.draw())
        val logU = log(u)
        if (logU <= math.log1p(-shape)) {
          val logV = log(v)
          val logX = logU / shape
          if (logX <= logV) logX
          else rec
        } else {
          val y = -log((1 - u) / shape)
          val logX = math.log(1.0 - shape + shape * y) / shape
          if (logX <= math.log(v + y)) logX
          else rec
        }
      }
      rec + math.log(scale)
    } else math.log(draw)

  def draw() = {
    if (shape == 1.0) {
      scale * -math.log(rand.uniform.draw())
    } else if (shape < 1.0) {
      // from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
      @tailrec
      def rec: Double = {
        val u = rand.uniform.draw()
        val v = -math.log(rand.uniform.draw())
        if (u <= 1.0 - shape) {
          val x = pow(u, 1.0 / shape)
          if (x <= v) x
          else rec
        } else {
          val y = -log((1 - u) / shape)
          val x = pow(1.0 - shape + shape * y, 1.0 / shape)
          if (x <= (v + y)) x
          else rec
        }
      }

      scale * rec
//      val c = 1.0 + shape/scala.math.E
//      var d = c * rand.uniform.draw()
//      var ok = false
//      var x = 0.0
//      while(!ok) {
//        if (d >= 1.0) {
//          x = -log((c - d) / shape)
//          if (-math.log(rand.uniform.draw()) >= (1.0 - shape) * log(x)) {
//            x = (scale * x)
//            ok = true
//          }
//        } else {
//          x = math.pow(d, 1.0/shape)
//          if (-math.log(rand.uniform.draw()) >= (1.0 - shape) * log(x)) {
//            x = scale * x
//            ok = true
//          }
//        }
//        d = c * rand.uniform.draw()
//      }
//      x
    } else {
      // from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
      val d = shape - 1.0 / 3.0
      val c = 1.0 / math.sqrt(9.0 * d)
      var r = 0.0
      var ok = false
      while (!ok) {
        var v = 0.0
        var x = 0.0
        do {
          x = rand.generator.nextGaussian
          v = 1.0 + c * x
        } while (v <= 0)

        v = v * v * v
        val x2 = x * x
        val u = rand.uniform.draw()
        if (u < 1.0 - 0.0331 * (x2 * x2)
          || log(u) < 0.5 * x2 + d * (1.0 - v + log(v))) {
          r = (scale * d * v)
          ok = true
        }
      }
      r
    }
  }

  def mean = shape * scale
  def variance = mean * scale
  def mode = { require(shape >= 1); mean - scale }
  def entropy = logNormalizer - (shape - 1) * digamma(shape) + shape

  override def probability(x: Double, y: Double): Double = {
    new GammaDistribution(shape, scale).probability(x, y)
  }

  override def inverseCdf(p: Double): Double = {
//    gammp(this.shape, p / this.scale);
    new GammaDistribution(shape, scale).inverseCumulativeProbability(p)
  }

  override def cdf(x: Double): Double = {
    new GammaDistribution(shape, scale).cumulativeProbability(x)
  }
}

object Gamma extends ExponentialFamily[Gamma, Double] with ContinuousDistributionUFuncProvider[Double, Gamma] {
  type Parameter = (Double, Double)
  import breeze.stats.distributions.{SufficientStatistic => BaseSuffStat}
  case class SufficientStatistic(n: Double, meanOfLogs: Double, mean: Double)
      extends BaseSuffStat[SufficientStatistic] {
    def *(weight: Double) = SufficientStatistic(n * weight, meanOfLogs, mean)
    def +(t: SufficientStatistic) = {
      val delta = t.mean - mean
      val newMean = mean + delta * (t.n / (t.n + n))
      val logDelta = t.meanOfLogs - meanOfLogs
      val newMeanLogs = meanOfLogs + logDelta * (t.n / (t.n + n))
      SufficientStatistic(t.n + n, newMeanLogs, newMean)
    }
  }

  def emptySufficientStatistic = SufficientStatistic(0, 0, 0)

  def sufficientStatisticFor(t: Double) = SufficientStatistic(1, math.log(t), t)

  // change from Timothy Hunter. Thanks!
  def mle(ss: SufficientStatistic) = {
    val s = math.log(ss.mean) - ss.meanOfLogs
    assert(s > 0, s) // check concavity
    val k_approx = approx_k(s)
    assert(k_approx > 0, k_approx)
    val k = Nwt_Rph_iter_for_k(k_approx, s)
    val theta = ss.mean / (k)
    (k, theta)
  }
  /*
   * s = log( x_hat) - log_x_hat
   */
  def approx_k(s: Double): Double = {
    // correct within 1.5%
    (3 - s + math.sqrt(math.pow((s - 3), 2) + 24 * s)) / (12 * s)
  }

  private val MaxIter = 50

  private def Nwt_Rph_iter_for_k(k: Double, s: Double, iter: Int = 0): Double = {
    /*
     * For a more precise estimate, use Newton-Raphson updates
     */
    val k_new = k - (math.log(k) - digamma(k) - s) / (1.0 / k - trigamma(k))
    if (closeTo(k, k_new) || iter >= MaxIter)
      k_new
    else
      Nwt_Rph_iter_for_k(k_new, s, iter + 1)
  }

  def distribution(p: Parameter) = new Gamma(p._1, p._2)

  def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[(Double, Double)] {
    val SufficientStatistic(n, meanOfLogs, mean) = stats
    def calculate(x: (Double, Double)) = {
      val (a, b) = x
      val obj = -n * ((a - 1) * meanOfLogs - lgamma(a) - a * log(b) - mean / b)
      val gradA = -n * (meanOfLogs - digamma(a) - log(b))
      val gradB = -n * (-a / b + mean / b / b)
      (obj, (gradA, gradB))
    }
  }
}
