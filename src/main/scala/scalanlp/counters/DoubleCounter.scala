package scalanlp.counters;
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

import scalala.Scalala._;
import scalala.tensor._;
import scalala.tensor.sparse._;
import scalala.collection._;
import scalanlp.collection.mutable._

import scalanlp.util._;

/**
* The basic interface for Counters, which are sort of likes maps from T to Double.
*
* A Counter is a Tensor1 with type T, backed by a an Index (hence, Indexed[T])
* and a scalala.Vector (default: SparseHashVector). 
*
* Normally you will want to use object Counters's for creating Counters.
*
* @author dlwh
*/
trait BaseDoubleCounter[T] extends Tensor1[T] with TrackedStatistics[T] { outer =>
  /**
  * A counter should be backed by a DoubleValuedMap, of any sort.
  * (We use DoubleValuedMap to get around excessive boxing.)
  * Default value should defer to BaseDoubleCounter.default
  */
  protected val counts:DoubleValuedMap[T] = new DoubleValuedMap.FastAny2DoubleMap [T] {
    override def default(k: T) = outer.default;
  }

  val activeDomain : MergeableSet[T] = new MergeableSet[T] {
    def contains(t:T) = counts.contains(t);
    def iterator = counts.keysIterator;
    
  }
  val domain : MergeableSet[T] = activeDomain;

  /**
  * Sets the count for t to be v, and calls updateStatistics.
  */
  def update(t: T, v: Double) {
    val oldV = counts(t);
    if(v == default)
      counts -= t;
    else 
      counts(t) = v;
    updateStatistics(t,oldV,v);
  }

  /**
  * Returns the number of keys with stored values.
  */
  override def size = counts.size;

  /**
  * Returns Some(v) if this.contains(t), else None
  */
  def get(t: T) = counts.get(t);

  /**
  * Returns true if we store a (likely) non-default value for t
  */
  def contains(t:T) = counts contains t;

  /**
  * Returns the value associated with t, or default if none is.
  */
  def apply(t: T) = counts(t);

  /**
  * Equivalent to this(t) += inc. it may be faster (though at the moment it isn't)
  */
  def incrementCount(t:T, inc: Double) {
    val oldV = counts(t);
    counts(t) = inc + oldV;
    updateStatistics(t,oldV,inc + oldV);
  }

  /**
  * Adds each element of the iterable to the counter.
  */
  def ++=(kv: Iterable[(T,Double)]) = kv.foreach(+=);
  /**
  * Adds each element of the iterable to the counter.
  */
  def ++=(kv: Iterator[(T,Double)]) = kv.foreach(+=);

  /**
  * Equivalent to this(kv._1) += kv._2. (i.e., it increments the count for kv._1 by kv._2)
  */
  def +=(kv: (T,Double)) { this(kv._1) += kv._2 }

  /**
   * Return the T with the largest count
   */
  def argmax: T = {
    (counts reduceLeft ((p1:(T,Double),p2:(T,Double)) => if(p1._2 > p2._2) p1 else p2))._1
  }

  /**
   * Return the T with the smallest count
   */
  def argmin: T = {
    (counts reduceLeft ((p1:(T,Double),p2:(T,Double)) => if(p1._2 < p2._2) p1 else p2))._1
  }

  /**
   * Return the largest count
   */
  def max: Double = counts.valuesIterator reduceLeft (_ max _)

  /**
   * Return the smallest count
   */
  def min: Double = counts.valuesIterator reduceLeft (_ min _)

  protected[counters] override def ensure(otherDomain: PartialMap[T,Double]) {
    // no need to do anything here.
  }

  /**
  * For each k,v in the map, it sets this(k) = f(k,v)
  */
  def transform(f: (T,Double)=>Double) {
    for( (k,v) <- activeElements) {
      update(k,f(k,v));
    }
  }

  def map[U](f: ((T,Double))=>U):Iterable[U] = {
    activeElements.map{ kv => f(kv)} toSequence; 
  }

  def map[U](f: (T,Double)=>U):Iterable[U] = {
    activeElements.map{ case (k,v) => f(k,v)} toSequence; 
  }
}
