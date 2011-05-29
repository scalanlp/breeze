package scalanlp.stats.distributions;

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

import scalanlp.math.Bessel;
import scalala.library.Numerics;
import math._
import scalala.tensor.Counter
import scalala.tensor.dense.DenseVector
import scalanlp.optimize.{LBFGS, DiffFunction}
;

/**
 * Represents a Von Mises distribution, which is
 * a distribution over angles.
 *
 * @param mu is the mean of the distribution, ~ gaussian mean
 * @param k is the concentration, which is like 1/gaussian variance
 *
 * @author dlwh
 */
case class VonMises(mu: Double, k: Double)(implicit rand: RandBasis=Rand) extends ContinuousDistr[Double] with Moments[Double] {
  require( k >= 0, "K must be positive");
  require(mu <= math.Pi * 2 && mu >= 0, "Mu must be in the range [0,2pi]");

  override def unnormalizedLogPdf(theta:Double) = cos(theta - mu) * k;
  val logNormalizer = math.log(Bessel.i0(k) * 2* Pi);

  private val r = { 
    val tau = 1.0 + sqrt(1.0 + 4.0 * k *k);
    val rho = (tau - sqrt(2.0 * tau)) / (2.0*k);
    (1.0 + rho * rho) / (2 * rho);
  }

  // rejection sampler based on the colt implementation
  private val myRandom = for {
    v <- rand.uniform;
    u <- rand.uniform;
    z = cos(Pi * u);
    w = (1.0 + r* z) / (r+z);
    c = k * (r - w);
    accept = v < (c * (2.0 - c)) || v <= c * exp(1.0-c)
    if accept
    choice <- rand.uniform
    theta = if(choice > 0.5) mu + acos(w) else mu -acos(w)
  } yield theta

  def draw = {
    myRandom.draw
  }
  
  override lazy val toString = "VonMises(mu=" + mu + ", k=" + k + ")";

  def mean = mu
  def mode = mean
  def variance = 1 - Bessel.i1(k) / Bessel.i0(k)
  def entropy = -k * Bessel.i1(k) / Bessel.i0(k) + math.log(2 * math.Pi * Bessel.i0(k))
}

object VonMises extends ExponentialFamily[VonMises,Double] {
  type Parameter = (Double,Double)
  case class SufficientStatistic(n: Double, sines: Double, cosines: Double) extends scalanlp.stats.distributions.SufficientStatistic[SufficientStatistic] {
    def +(t: SufficientStatistic) = new SufficientStatistic(n + t.n, sines + t.sines, cosines + t.cosines);

    def *(weight: Double) = SufficientStatistic(weight * n, weight * sines, weight * cosines);
  }

  def emptySufficientStatistic = SufficientStatistic(0,0,0);


  def sufficientStatisticFor(t: Double) = SufficientStatistic(1,sin(t),cos(t));
  def distribution(p: Parameter) = new VonMises(p._1,p._2)


  def mle(stats: SufficientStatistic): (Double, Double) = {
    val lensed = likelihoodFunction(stats).throughLens[DenseVector[Double]];
    val lbfgs = new LBFGS[DenseVector[Double]](100,3)
    // Starting points due to Sra, 2009)
    // http://en.wikipedia.org/wiki/Von_Mises-Fisher_distribution
    val startingMu = asin(stats.sines/stats.n);
    val rhat = sqrt(stats.sines * stats.sines + stats.cosines * stats.cosines) / stats.n
    val startingK = rhat * (2 - rhat * rhat) / (1-rhat * rhat);
    println(startingMu,startingK);
    val result = lbfgs.minimize(lensed,DenseVector(startingMu,startingK));
    val res@(a,b) = (result(0),result(1));
    res
  }

  def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[(Double,Double)] {
    def calculate(x: (Double,Double)) = {
      val DELTA = 1E-5
      val (mu,k) = x;
      if( mu < 0 || mu > 2*Pi || k < 0) (Double.PositiveInfinity,(0.0,0.0))
      else {
        val (sinx,cosx) = (sin(mu),cos(mu));
        val bessel_k = Bessel.i0(k)
        val logprob = stats.n * math.log(bessel_k * 2* Pi) - (stats.sines * sinx + stats.cosines * cosx)*k
        val mugrad = -k * (stats.sines * cos(mu) - stats.cosines * sin(mu))
        val kgrad = stats.n * (Bessel.i1(k)/bessel_k)  - (stats.sines * sinx + stats.cosines * cosx)

        (logprob,(mugrad,kgrad));

      }

    }
  }





  /*

  /**
   * Returns the maximum likelihood estimate of this distribution
   * For the given observations with (possibly pseudo-)counts
   */

  def mle(obs: Counter[Double,Double]) = {
    val sufStats = for {
      (o,count) <- obs.pairs
    } yield {
      (count * cos(o),count * sin(o)) 
    }
    val cosineSum = sufStats.iterator.map(_._1) reduceLeft(_ + _);
    val sineSum = sufStats.iterator.map(_._2) reduceLeft( _ + _ );
    val muPart = signum(cosineSum) * signum(sineSum) * atan(abs(sineSum/cosineSum));
    val mu = (muPart + {
      if(cosineSum < 0) Pi
      else if (cosineSum > 0 && sineSum < 0) 2 * Pi;
      else 0.0 
    } ) % (2 * Pi)
    
    val t = sqrt(pow(cosineSum/obs.sum,2) + pow(sineSum / obs.sum,2));
    val k = (1.28 - 0.53*pow(t,2)) * tan(Pi/2*t)
    
    /*
    val kx = {
      if(t < 0.53) t * (2 + t *t * (1 + 5 * t * t / 6))
      else if(t < 0.85) -0.4 + 1.39 * t + (0.43)/(1-t);
      else 1/( t* (3 + t * (-4 + t)));
    } */
    VonMises(mu,k)
  }
  */
}

