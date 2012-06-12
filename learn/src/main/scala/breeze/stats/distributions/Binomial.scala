package breeze.stats.distributions
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

import scalala.library.Numerics.lgamma;
import math._
import breeze.collection.immutable.BinomialHeap
import breeze.stats.distributions
import breeze.util._
import breeze.optimize.DiffFunction

/**
* A binomial distribution returns how many coin flips out of n are heads,
* where numYes is the probability of any one coin being heads.
*
* @author dlwh
* @param n is the number of coin flips
* @param numYes the probability of any one being true
*/
class Binomial(n: Int, p: Double)(implicit rand: RandBasis=Rand) extends DiscreteDistr[Int] with Moments[Double] {
  require(n > 0, "n must be positive!");
  require(p >= 0.0, "p must be non-negative!");
  def probabilityOf(k: Int) = exp(logProbabilityOf(k));

  override def toString() = "Binomial(" + n + ", " + p + ")";


  override def logProbabilityOf(k: Int) = {
    require(n >= k);
    require(k >= 0);
    lgamma(n+1) - lgamma(k+1) - lgamma(n-k+1) + k * log(p) + (n-k) * log(1-p)
  }
  
  override def draw() = {
    (1 to n).map(_ => if(rand.uniform.get < p) 1 else 0).foldLeft(0)(_+_);
  }

  def mean = n * p;
  def variance = mean * (1 - p);
  def mode = math.floor((n + 1)*p);
  /** with an additive O(1/n) term */
  def entropy = .5 * math.log(2 * math.Pi * variance);

}

