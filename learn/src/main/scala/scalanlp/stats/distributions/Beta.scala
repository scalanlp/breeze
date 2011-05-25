package scalanlp.stats.distributions
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

import math._;

import scalala.library.Numerics.{lgamma,digamma}
import scalala.tensor.Counter

/**
 * The Beta distribution, which is the conjugate prior for the Bernoulli distribution
 *
 * @author dlwh
 * @param a the number of pseudo-observations for false
 * @param b the number of pseudo-observations for true
 */
class Beta(a: Double, b: Double)(implicit rand: RandBasis = Rand) extends ContinuousDistr[Double] 
    with ConjugatePrior[Double, Boolean] with Moments[Double] {
  require(a > 0.0);
  require(b > 0.0);

  override def unnormalizedLogPdf(x: Double) = {
    require(x >= 0);
    require(x <= 1)
    (a-1) * log(x) + (b-1) * log(1-x)
  }
  
  val logNormalizer =  lgamma(a) + lgamma(b) - lgamma(a+b)
  
  private val aGamma = new Gamma(a,1)(rand);
  private val bGamma = new Gamma(b,1)(rand);
  
  override def draw() = {
    val ad = aGamma.get;
    val bd = bGamma.get;
    (ad) / (ad + bd);
  }
  
  override def posterior(obs: Iterator[(Boolean,Int)]) = {
    val ctr = Counter(obs.map{ case (k,v) => (k,v.toDouble)}.toSeq:_*);
    new Beta(ctr(true)+a, ctr(false)+b)(rand)
  }
  
  def predictive = {
    val ctr = Counter[Boolean,Double]();
    ctr(true) = a;
    ctr(false) = b;
    new Polya(ctr)(rand);
  }

  def mean = a / (a + b);
  def variance = (a * b) / ( (a + b) * (a+b) * (a+b+1));
  def mode = (a - 1) / (a+b - 2);
  def entropy = logNormalizer - (a - 1) * digamma(a) - (b-1) * digamma(b) + (a + b - 2) * digamma(a + b);
  

}
