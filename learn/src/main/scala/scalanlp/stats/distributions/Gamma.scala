package scalanlp.stats
package distributions;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

import scalala.library.Numerics._;
import math._
import scalala.tensor.dense.DenseVector
import scalanlp.optimize.{LBFGS, DiffFunction}


/**
 * Represents a Gamma distribution.
 * E[X] = shape * scale
 *
 * @author dlwh
 */
case class Gamma(val shape : Double, val scale : Double)(implicit rand: RandBasis = Rand)
    extends ContinuousDistr[Double] with Moments[Double] {
  if(shape <= 0.0 || scale <= 0.0)
    throw new IllegalArgumentException("Shape and scale must be positive");

  val logNormalizer = lgamma(shape) + shape * log(scale);

  override def unnormalizedLogPdf(x : Double) = (shape - 1) * log(x) - x/scale;

  override def toString = "Gamma(" + shape + "," + scale + ")";

  // Copied from Teh
  def draw() : Double = { 
    var aa = 0.0;
    var bb = 0.0;
    var cc = 0.0;
    var dd = 0.0;
    var uu = 0.0;
    var vv = 0.0;
    var ww = 0.0;
    var xx = 0.0;
    var yy = 0.0;
    var zz = 0.0;
    if (shape == 1.0) {
      /* Exponential */
      scale * -math.log(rand.uniform.get);
    } else if (shape < 1.0) {
      /* Use Johnks generator */
      cc = 1.0 / shape;
      dd = 1.0 / (1.0 - shape);
      while (true) {
        xx = pow(rand.uniform.get(), cc);
        yy = xx + pow(rand.uniform.get(), dd);
        if (yy <= 1.0) {
          return scale * -log(rand.uniform.get()) * xx / yy;
        }
      }
      sys.error("shouldn't get here");
    } else { /* shape > 1.0 */
      /* Use bests algorithm */
      bb = shape - 1.0;
      cc = 3.0 * shape - 0.75;
      while (true) {
        uu = rand.uniform.get();
        vv = rand.uniform.get();
        ww = uu * (1.0 - uu);
        yy = math.sqrt(cc / ww) * (uu - 0.5);
        xx = bb + yy;
        if (xx >= 0) {
          zz = 64.0 * ww * ww * ww * vv * vv;
          if ((zz <= (1.0 - 2.0 * yy * yy / xx))
              || (math.log(zz) <= 2.0 * (bb * math.log(xx / bb) - yy))) {
            return xx * scale;
          }
        }
      }
      sys.error("shouldn't get here");
    }
  }

  def mean = shape * scale;
  def variance = mean * scale;
  def mode = { require(shape >= 1); mean - scale}
  def entropy = logNormalizer - (shape - 1) * digamma(shape) + shape
}

object Gamma extends ExponentialFamily[Gamma,Double] {
  type Parameter = (Double,Double)
  import scalanlp.stats.distributions.{SufficientStatistic=>BaseSuffStat}
  case class SufficientStatistic(n: Double, meanOfLogs: Double, mean: Double) extends BaseSuffStat[SufficientStatistic] {
    def *(weight: Double) = SufficientStatistic(n*weight, meanOfLogs*weight, mean * weight);
    def +(t: SufficientStatistic) = {
      val delta = t.mean - mean;
      val newMean = mean + delta * (t.n /(t.n + n));
      val logDelta = t.meanOfLogs - meanOfLogs
      val newMeanLogs = meanOfLogs + logDelta * (t.n /(t.n + n));
      SufficientStatistic(t.n+n, newMeanLogs, newMean);
    };
  }

  def emptySufficientStatistic = SufficientStatistic(0,0,0);

  def sufficientStatisticFor(t: Double) = SufficientStatistic(1,math.log(t),t);

  def mle(stats: SufficientStatistic): (Double, Double) = {
    val lensed = likelihoodFunction(stats).throughLens[DenseVector[Double]];
    val lbfgs = new LBFGS[DenseVector[Double]](100,3)
    // Starting points due to Minka
    // http://research.microsoft.com/en-us/um/people/minka/papers/minka-gamma.pdf
    // shockingly good
    val startingA = .5 / ( math.log(stats.mean) - stats.meanOfLogs)
    val startingB = stats.mean / startingA
    val result = lbfgs.minimize(lensed,DenseVector(startingA,startingB));
    val res@(a,b) = (result(0),result(1));
    res
  }


  def distribution(p: Parameter) = new Gamma(p._1,p._2);

  def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[(Double,Double)] {
    val SufficientStatistic(n,meanOfLogs,mean) = stats;
    def calculate(x: (Double,Double)) = {
      val (a,b) = x;
      val obj = -n * ((a - 1) * meanOfLogs - lgamma(a) - a * log(b)- mean / b)
      val gradA = -n * (meanOfLogs - digamma(a)  - log(b));
      val gradB = -n * (-a/b + mean / b / b);
      (obj,(gradA,gradB));
    }
  }
}
