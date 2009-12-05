package scalanlp.stats.sampling;

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

import cern.jet.math.Bessel;
import scalanlp.math.Numerics;
import scalanlp.counters.Counters._;
import Numerics._;
import Math._;

/**
 * Represents a Von Mises distribution, which is
 * a distribution over angles.
 *
 * @param mu is the mean of the distribution, ~ gaussian mean
 * @param k is the concentration, which is like 1/gaussian variance
 *
 * @author dlwh
 */
case class VonMises(val mu: Double, val k: Double)(implicit rand: RandBasis=Rand) extends ContinuousDistr[Double] {
  require( k >= 0, "K must be postive");
  require(mu <= Math.Pi * 2 && mu >= 0, "Mu must be in the range [0,2pi]");

  override def unnormalizedLogPdf(theta:Double) = cos(theta - mu) * k;
  val logNormalizer = Math.log(Bessel.i0(k) * 2* Pi);

  private val r = { 
    val tau = 1.0 + sqrt(1.0 + 4.0 * k *k);
    val rho = (tau - sqrt(2.0 * tau)) / (2.0*k);
    (1.0 + rho * rho) / (2 * rho);
  }

  // rejection sampler based on the colt implementation
  private val myRandom = for {
    u <- rand.uniform;
    v <- rand.uniform;
    z = cos(Pi * u);
    w = (1.0 + r* z) / (r+z);
    c = k * (r - w);
    reject = (c * (2.0 - c) < v) && (log(c/v) + 1.0 < c)
    if !reject
    choice <- rand.uniform
    theta = if(choice > 0.5) mu + acos(w) else mu -acos(w)
  } yield theta % (2 * Pi);

  /**
   * This RNG seems to over-disperse the draws, based on
   * several different maximum likelihood estimates. The
   * dispersion is worse for large k. 
   */
  def draw = {
    myRandom.draw
  }
  
  override lazy val toString = "VonMises(mu=" + mu + ", k=" + k + ")";
}

object VonMises {
  /**
   * Returns the maximum likelihood estimate of this distribution
   * For the given observations with (possibly pseudo-)counts
   */
  def mle(obs: DoubleCounter[Double]) = {
    val sufStats = for {
      (o,count) <-  obs
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
    
    val t = sqrt(pow(cosineSum/obs.total,2) + pow(sineSum / obs.total,2));
    val k = (1.28 - 0.53*pow(t,2)) * tan(Pi/2*t)
    
    /*
    val kx = {
      if(t < 0.53) t * (2 + t *t * (1 + 5 * t * t / 6))
      else if(t < 0.85) -0.4 + 1.39 * t + (0.43)/(1-t);
      else 1/( t* (3 + t * (-4 + t)));
    } */
    VonMises(mu,k)
  }
}

