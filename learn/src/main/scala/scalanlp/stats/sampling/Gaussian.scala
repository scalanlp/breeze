package scalanlp.stats
package sampling;

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


import Rand._;
import math._;
import scalala.library.Numerics._
import scalanlp.stats.expfam.ExponentialFamily
import scalanlp.optimize.DiffFunction
;

/**
 * Represents a Gaussian distribution over a single real variable.
 * 
 * @author dlwh
 */
class Gaussian(val mu :Double, val sigma : Double)(implicit rand: RandBasis = Rand)
    extends ContinuousDistr[Double] with Moments[Double] {
  private val inner = rand.gaussian(mu,sigma);
  def draw() = inner.get();

  private val sqrt2 = math.sqrt(2.0);

  /**
  * Computes the inverse cdf of the p-value for this gaussian.
  * 
  * @param p: a probability in [0,1]
  * @return x s.t. cdf(x) = p
  */
  def icdf(p: Double) = {
    require( p >= 0 );
    require( p <= 1 );

    mu + sigma * sqrt2 * erfi(2 * p - 1);
  }

  /**
  * Computes the cumulative density function of the value x.
  */
  def cdf(x: Double) = .5 * (1 + erf( (x - mu)/sqrt2 / sigma));

  override def unnormalizedLogPdf(t: Double) = { 
    val d = (t - mu)/sigma; 
    d *d 
  } 
  
  val normalizer = 1.0/sqrt(2 * Pi) / sigma;
  val logNormalizer = log(sqrt(2 * Pi)) + log(sigma);

  def mean = mu;
  def variance = sigma * sigma;
}

object Gaussian extends ExponentialFamily[Gaussian,Double,(Double,Double)] {
  import expfam.{SufficientStatistic=>BaseSuffStat}
  final case class SufficientStatistic(n: Double, mean: Double, M2: Double) extends BaseSuffStat[SufficientStatistic] {
    def *(weight: Double) = SufficientStatistic(n * weight, sum * weight, M2 * weight)

    // Due to Chan
    def +(t: SufficientStatistic) = {
      val delta = t.mean - mean;
      val newMean = mean + delta * (t.n / (t.n + n));
      val newM2 = M2 + t.M2 + delta * delta * (t.n * n) / (t.n + n);
      SufficientStatistic(t.n + n, newMean, newM2);
    }

    def variance = M2/ (n-1);
  }

  val emptySufficientStatistic = SufficientStatistic(0,0,0)

  def sufficientStatisticFor(t: Double) = {
    SufficientStatistic(1,t,0);
  }

  def mle(stats: SufficientStatistic) = (stats.mean,stats.variance);

  def distribution(p: (Double, Double)) = new Gaussian(p._1,p._2)

  def likelihoodFunction(stats: SufficientStatistic):DiffFunction[(Double,Double)] = new DiffFunction[(Double,Double)] {
    def calculate(x: (Double, Double)) = {
      val objective =
    }
  }
}