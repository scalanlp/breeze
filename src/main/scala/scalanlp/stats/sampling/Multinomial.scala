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
import math.Numerics._;

import scalanlp.util.Log;

/**
 * Represents a multinomial Distribution over elements.
 */
trait Multinomial[T] extends DiscreteDistr[T] {
  protected def components : DoubleCounter[T];
  protected def total : Double;

  // check rep
  for ((k,v) <- components.elements) {
    if (v < 0) {
      throw new IllegalArgumentException("Multinomial has negative mass at index "+k);
    }
  }
  
  def draw() = {
    var prob = Rand.uniform.get() * total;
    if(prob.isNaN) {
      Log.globalLog(Log.ERROR)("You got a NaN!");
    }
    val elems = components.elements;
    var (e,w) = elems.next;
    prob  = prob - w;
    while(prob > 0) {
      val t  = elems.next;
      e = t._1;
      prob = prob - t._2;
    }
    e
  }

  /**
   * Returns a Multinomial(x) \propto  this(x) * that(x);
   */
  def *[U>:T](that : Multinomial[U]) = {
    val c :DoubleCounter[U] = aggregate(that.components);
    for((x,w) <- components
      if c.get(x) != None)
        c(x) *= w/ (c.total * total);
    Multinomial(c);    
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
  def apply[T](c : DoubleCounter[T])  = new Multinomial[T] {
    def total = c.total;
    if(total.isNaN || total <= 0.) throw new IllegalArgumentException("total is " + total);
    protected def components = c;
  }

  /**
   * Returns a Multinomial where the probability of each element in the counter
   * is proportional to its count.
   */
  def fromCounter[T](c:DoubleCounter[T]) = apply(c);
  
  /**
   * Returns a Multinomial where the probability is proportional to a(i)
   */
  def apply(a : Array[Double]) : Multinomial[Int] = apply(a,a.foldLeft(0.0)(_+_));

  /**
   * Returns a Multinomial where the probability is proportional to a(i).
   * Takes the total for speed.
   */
  def apply(arr : Array[Double], t: Double) = new Multinomial[Int] {
    lazy val components = {
      val c = DoubleCounter[Int];
      c ++= arr.zipWithIndex.map(_.swap);
      c;
    }
    def total = t;
  }
}
