package scalanlp.stats.sampling
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

/*
 * A Bernoulli distribution represents a distribution over weighted coin flips, with p(true) = p, the
 *
 * @author dlwh
 * @param p the probability of true
 */
class Bernoulli(p: Double, rand: RandBasis = Rand) extends DiscreteDistr[Boolean] with Moments[Double] {
  require(p >= 0.0);
  require(p <= 1.0);
  def probabilityOf(b: Boolean) = if(b) p else (1-p)
  
  override def draw() = {
    rand.uniform.get < p
  }

  def mean = p;
  def variance = p * (1-p);
}
