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

import scalanlp.util.Log
import scalala.tensor.Counter

/**
 * Represents a multinomial Distribution over elements.
 *
 * @author dlwh
 */
trait Multinomial[T] extends DiscreteDistr[T] {
  protected def components : Counter[T,Double];
  protected def sum : Double;
  protected implicit val rand: RandBasis;

  // check rep
  for ((k,v) <- components.pairsIterator) {
    if (v < 0) {
      throw new IllegalArgumentException("Multinomial has negative mass at index "+k);
    }
  }
  
  def draw() = {
    var prob = rand.uniform.get() * sum;
    if(prob.isNaN) {
      Log.globalLog(Log.ERROR)("You got a NaN!");
    }
    val elems = components.pairsIterator;
    var (e,w:Double) = elems.next;
    prob  = prob - w;
    while(prob > 0) {
      val t  = elems.next;
      e = t._1;
      prob = prob - t._2;
    }
    e
  }

  def probabilityOf(e : T) = components.apply(e) / sum;
  def logProbabilityOf(c: Counter[T,Double]) = {
    val probs = for( (k,v) <- c.pairs) yield v * math.log(components.apply(k) / sum);
    probs.foldLeft(0.0)(_+_);
  }
  override def unnormalizedProbabilityOf(e:T) = components.apply(e);
  
  override def toString = components.pairsIterator.mkString("Multinomial{",",","}")
}

/** 
 * Provides routines to create Multinomial
 * @author(dlwh)
 */
object Multinomial {

  /**
   * Returns a Multinomial where the probability of each element in the counter
   * is proportional to its count.
   */
  def apply[T](c : Counter[T,Double])(implicit r: RandBasis=Rand)  = new Multinomial[T] {
    val sum = c.sum;
    if(sum.isNaN || sum <= 0.) throw new IllegalArgumentException("sum is " + sum);
    protected def components = c;
    protected val rand = r;
  }

  /**
   * Returns a Multinomial where the probability of each element in the counter
   * is proportional to its count.
   */
  def fromCounter[T](c:Counter[T,Double])(implicit rand: RandBasis=Rand) = apply(c)(rand);

  /**
   * Returns a Multinomial where the probability is proportional to a(i).
   * Takes the sum for speed.
   */
  def apply(arr : Array[Double], t: Double)(implicit r: RandBasis) = new Multinomial[Int] {
    lazy val components = {
      val c = Counter[Int,Double](arr.zipWithIndex.map(_.swap):_*)
      c;
    }
    val rand = r;
    def sum = t;
  }
}
