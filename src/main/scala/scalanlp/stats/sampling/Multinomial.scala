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

import scalanlp.counters.Counters._;
import scalanlp.counters._;
import scalanlp.math.Numerics._;

import scalanlp.util.Log;

/**
 * Represents a multinomial Distribution over elements.
 *
 * @author dlwh
 */
trait Multinomial[T] extends DiscreteDistr[T] {
  protected def components : DoubleCounter[T];
  protected def total : Double;
  protected implicit val rand: RandBasis;

  // check rep
  for ((k,v) <- components.iterator) {
    if (v < 0) {
      throw new IllegalArgumentException("Multinomial has negative mass at index "+k);
    }
  }
  
  def draw() = {
    var prob = rand.uniform.get() * total;
    if(prob.isNaN) {
      Log.globalLog(Log.ERROR)("You got a NaN!");
    }
    val elems = components.iterator;
    var (e,w:Double) = elems.next;
    prob  = prob - w;
    while(prob > 0) {
      val t  = elems.next;
      e = t._1;
      prob = prob - t._2;
    }
    e
  }

  def probabilityOf(e : T) = components.apply(e) / total;
  def logProbabilityOf(c: DoubleCounter[T]) = {
    val probs = for( (k,v) <- c) yield v * Math.log(components.apply(k) / total);
    probs.foldLeft(0.0)(_+_);
  }
  override def unnormalizedProbabilityOf(e:T) = components.apply(e);
  
  override def toString = components.mkString("Multinomial{",",","}")
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
  def apply[T](c : DoubleCounter[T])(implicit r: RandBasis=Rand)  = new Multinomial[T] {
    def total = c.total;
    if(total.isNaN || total <= 0.) throw new IllegalArgumentException("total is " + total);
    protected def components = c;
    protected val rand = r;
  }

  /**
   * Returns a Multinomial where the probability of each element in the counter
   * is proportional to its count.
   */
  def apply[T](c : LogCounters.DoubleCounter[T])(implicit r:RandBasis) = new Multinomial[T] {
    import Math._;
    if(total.isNaN ) throw new IllegalArgumentException("total is " + total);
    def total = exp(c.logTotal);
    protected lazy val components = LogCounters.normalize(c);
    override def draw() = {
      var prob = Math.log(rand.uniform.get())+ c.logTotal;
      if(prob.isNaN) {
        Log.globalLog(Log.ERROR)("You got a NaN!");
      }
      val elems = c.iterator;
      var (e,w:Double) = elems.next;
      prob  = scalanlp.math.Numerics.logDiff(prob,w);
      while(prob > Double.NegativeInfinity) {
        val t  = elems.next;
        e = t._1;
        prob  = scalanlp.math.Numerics.logDiff(prob,t._2);
      }
      e
    }

    protected val rand = r;
    override def probabilityOf(e : T) = exp(logProbabilityOf(e));
    override def logProbabilityOf(e: T) = c(e) - c.logTotal;
    override def unnormalizedLogProbabilityOf(e:T) = c(e);
    override def unnormalizedProbabilityOf(e:T) = exp(unnormalizedLogProbabilityOf(e));
    override def toString = c.mkString("LogMultinomial{",",","}")
  }


  /**
   * Returns a Multinomial where the probability of each element in the counter
   * is proportional to its count.
   */
  def fromCounter[T](c:DoubleCounter[T])(implicit rand: RandBasis=Rand) = apply(c)(rand);


  /**
   * Returns a Multinomial where the probability of each element in the counter
   * is proportional to its count.
   */
  def fromLogCounter[T](c:LogCounters.DoubleCounter[T])(implicit r: RandBasis=Rand) = apply(c)(r);
  
  
  /**
   * Returns a Multinomial where the probability is proportional to a(i)
   */
  def apply(a : Array[Double])(implicit rand: RandBasis) : Multinomial[Int] = apply(a,a.foldLeft(0.0)(_+_))(rand);

  /**
   * Returns a Multinomial where the probability is proportional to a(i).
   * Takes the total for speed.
   */
  def apply(arr : Array[Double], t: Double)(implicit r: RandBasis) = new Multinomial[Int] {
    lazy val components = {
      val c = DoubleCounter[Int];
      c ++= arr.zipWithIndex.map(_.swap);
      c;
    }
    val rand = r;
    def total = t;
  }
}
