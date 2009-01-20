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


import Rand._;
import Math._;

class Gaussian(val mu :Double, val sigma : Double) extends ContinuousDistr[Double] {
  private val inner = Rand.gaussian(mu,sigma);
  def draw() = inner.get();

  def pdf(t : Double) = exp(logPdf(t));
  override def logPdf(t :Double) =  unnormalizedLogPdf(t) + logNormalizer;
  override def unnormalizedLogPdf(t: Double) = { 
    val d = (t - mu)/sigma; 
    d *d 
  } 
  override def unnormalizedPdf(t: Double) = exp(unnormalizedLogPdf(t));
  
  val normalizer = 1.0/sqrt(2 * Pi) / sigma;
  val logNormalizer = log(1.0/sqrt(2 * Pi)) - log(sigma);
}
