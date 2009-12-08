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

import scala.collection.mutable.HashMap;
import scalala.Scalala._;
import scalala.tensor._;
import scalala.collection._;

/**
* Base class for classes like {#link Counters#PairedDoubleCounter}. Roughly, it is a 
* two-dimensional map/tensor/matrix with other counters as "rows."
*
* @author dlwh
*/
abstract class BasePairedDoubleCounter[K1,K2] 
    extends Tensor2[K1,K2] with TrackedStatistics[(K1,K2)] { outer =>

  type DoubleCounter <: BaseDoubleCounter[K2] with PairStatsTracker[K1,K2];
  protected def mkDoubleCounter(k1:K1): DoubleCounter;

  private val theMap = new HashMap[K1,DoubleCounter] {
    override def default(k1:K1) = getOrElseUpdate(k1,mkDoubleCounter(k1));
  }

  private val k1Set = scala.collection.mutable.Set[K1]();
  private val k2Set = scala.collection.mutable.Set[K2]();

  // Keep track of size, and what keys we know of.
  override protected[counters] def updateStatistics(k1k2: (K1,K2), oldV: Double, newV: Double) = {
    val DEFAULT = default;
    (oldV,newV) match {
      case (DEFAULT,DEFAULT) => (); 
      case (_,DEFAULT) => size_ -= 1;
      case (DEFAULT,_) => size_ += 1; k1Set += k1k2._1; k2Set += k1k2._2;
      case (_,_) =>
    }
    super.updateStatistics(k1k2, oldV, newV);
  }

  /**
  * Returns the total number of nondefault entries in the counter.
  */
  override def size = size_ ;
  private var size_ = 0;

  def domain = {
    ProductSet(MergeableSet(k1Set),MergeableSet(k2Set));
  }

  def rows = theMap.iterator;

  // todo: make this faster.
  val activeDomain = new MergeableSet[(K1,K2)] {
    def contains(c: (K1,K2)) = theMap.contains(c._1) && theMap(c._1).contains(c._2);

    def iterator = {
      for( (k1,c) <-theMap.iterator;
        k2 <- c.keysIterator)
      yield ( (k1,k2));
    }

    override def size = outer.size;
    
  }

  override def iterator = {
    for( (k1,c) <-theMap.iterator;
      (k2,v) <- c.iterator)
    yield ( (k1,k2),v);
  }

  def apply(k1 : K1) = getRow(k1);
  def get(k1 : K1, k2 : K2) : Option[Double] = theMap.get(k1).flatMap(_.get(k2))
  def apply(k1 : K1, k2: K2) : Double= apply(k1)(k2);
  def update(k1 : K1, k2: K2, v : Double) = apply(k1)(k2) = v;

  override def toString = {
    val b = new StringBuilder;
    b append "["
    foreach {  x=>
      b append x
      b append ",\n"
    }
    b append "]"
    b.toString
  }

  def getRow(k1:K1) = theMap(k1);
  def getCol(k2:K2) = {
    val result = Counters.DoubleCounter[K1]();
    for( (k1,c) <- theMap) {
      val v = c(k2);
      if(v != 0.0) {
        result(k1) = v;
      }
    }
    result;
  }

  /** 
   * Returns an iterator over each (K1,K2,Value) pair
   */ 
  def triples : Iterator[(K1,K2,Double)] = {
    for( (k1,c) <- theMap.iterator;
      (k2,v) <- c.iterator)
    yield (k1,k2,v);
  }

  def +=(that : Iterable[(K1,K2,Double)]) {
    this += that.iterator;
  }

  def +=(that : Iterator[(K1,K2,Double)]) {
    for( (k1,k2,v) <- that) {
      this(k1,k2) += v;
    }
  }

  protected[counters] override def ensure(otherDomain: PartialMap[(K1,K2),Double]) {
    // no need to do anything here.
  }

}

trait PairStatsTracker[K1,K2] extends TrackedStatistics[K2] {
  protected def outer: TrackedStatistics[(K1,K2)];
  protected def k1: K1;

  override protected[counters] def updateStatistics(k2: K2, oldV: Double, newV: Double) = {
    outer.updateStatistics( (k1,k2),oldV,newV);
    super.updateStatistics(k2,oldV,newV);
  }
}
