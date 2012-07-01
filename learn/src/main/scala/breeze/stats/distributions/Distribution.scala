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


/**
 * Represents an unnormalized probability distribution.
 * 
 * @author(dlwh)
 */
trait Measure[T] {
  /** Returns the unnormalized value of the measure*/
  def apply(x:T): Double;
  /** Returns the log unnormalized value of the measure*/
  def logApply(x:T): Double = apply(x);
}

/**
 * Represents a continuous Distribution.
 * Why T? just in case.
 * @author dlwh
 */
trait ContinuousDistr[T] extends Measure[T] with Rand[T] {
  /** Returns the probability density function at that point.*/
  def pdf(x: T): Double  = math.exp(logPdf(x))
  def logPdf(x:T): Double = unnormalizedLogPdf(x) - logNormalizer;
  /** Returns the probability density function up to a constant at that point.*/
  def unnormalizedPdf(x:T): Double = math.exp(unnormalizedLogPdf(x))
  
  def unnormalizedLogPdf(x:T): Double;
  def logNormalizer : Double;
  
  def apply(x:T) = unnormalizedPdf(x);
  override def logApply(x:T) = unnormalizedLogPdf(x);
}

/**
 * Represents a discrete Distribution.
 * @author dlwh
 */
trait DiscreteDistr[T] extends Measure[T] with Rand[T] {
  /** Returns the probability of that draw. */
  def probabilityOf(x: T): Double; 
  def logProbabilityOf(x:T): Double = math.log(probabilityOf(x));
  /** Returns the probability of that draw up to a constant */
  def unnormalizedProbabilityOf(x:T): Double = probabilityOf(x);
  def unnormalizedLogProbabilityOf(x:T): Double = math.log(unnormalizedProbabilityOf(x));
  
  def apply(x:T) = unnormalizedProbabilityOf(x);
  override def logApply(x:T) = unnormalizedLogProbabilityOf(x);
}

trait Moments[T] {
  def mean: T;
  def variance: T;
  def entropy: Double
  def mode: T
}

