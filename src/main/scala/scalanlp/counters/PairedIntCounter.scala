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
* Base class for classes like {#link PairedIntCounter}. Roughly, it is a 
* two-dimensional map/tensor/matrix with other counters as "rows."
*
* @author dlwh
*/
abstract class BasePairedIntCounter[K1,K2] 
    extends PairedIntCounter.CounterFactory[K1,K2] 
    with PartialMap[(K1,K2),Int] with TrackedIntStatistics[(K1,K2)] { outer =>

  private val theMap = new HashMap[K1,IntCounter] {
    override def default(k1:K1) = getOrElseUpdate(k1,mkIntCounter(k1));
  }

  private val k1Set = scala.collection.mutable.Set[K1]();
  private val k2Set = scala.collection.mutable.Set[K2]();

  // Keep track of size, and what keys we know of.
  statistics += { (k1k2: (K1,K2), oldV: Int, newV: Int) =>
    val DEFAULT = default;
    (oldV,newV) match {
      case (DEFAULT,DEFAULT) => (); 
      case (_,DEFAULT) => size_ -= 1;
      case (DEFAULT,_) => size_ += 1; k1Set += k1k2._1; k2Set += k1k2._2;
      case (_,_) =>
    }
  }

  /**
  * Returns the total number of nondefault entries in the counter.
  */
  override def size = size_ ;
  private var size_ = 0;

  def default = 0;

  def domain = {
    ProductSet(MergeableSet(k1Set),MergeableSet(k2Set));
  }

  // todo: make this faster.
  def activeDomain = theMap.foldLeft[MergeableSet[(K1,K2)]](EmptySet()) { (set,kc) =>
    val (k1,c) = kc;
    val s2 =Set() ++ (for(k2 <- c.activeKeys) yield (k1,k2) )
    UnionSet(set,MergeableSet(s2));
  }

  override def iterator = {
    for( (k1,c) <-theMap.iterator;
      (k2,v) <- c.iterator)
    yield ( (k1,k2),v);
  }

  def apply(k1 : K1) = getRow(k1);
  def get(k1 : K1, k2 : K2) : Option[Int] = theMap.get(k1).flatMap(_.get(k2))
  def apply(k1 : K1, k2: K2) : Int= apply(k1)(k2);
  def apply(k : (K1,K2)) : Int= apply(k._1)(k._2);
  def update(k1 : K1, k2: K2, v : Int) = apply(k1)(k2) = v;

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
    val result = Counters.IntCounter[K1]();
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
  def triples : Iterator[(K1,K2,Int)] = {
    for( (k1,c) <- theMap.iterator;
      (k2,v) <- c.iterator)
    yield (k1,k2,v);
  }

  def +=(that : Iterable[(K1,K2,Int)]) {
    this += that.iterator;
  }

  def +=(that : Iterator[(K1,K2,Int)]) {
    for( (k1,k2,v) <- that) {
      this(k1,k2) += v;
    }
  }
}

import PairedIntCounter._;

class PairedIntCounter[K1,K2] extends BasePairedIntCounter[K1,K2] 
    with TrackedIntStatistics.Total[(K1,K2)]
    with TotaledCounterFactory[K1,K2];

object PairedIntCounter {
  trait CounterFactory[K1,K2] { outer: BasePairedIntCounter[K1,K2] =>

    protected type SelfType[T1,T2] <: BasePairedIntCounter[T1,T2];
    protected def mkPairCounter[T1,T2] : SelfType[T1,T2];

    type IntCounter <: BaseIntCounter[K2] with PairStatsTracker;
    protected def mkIntCounter(k1:K1): IntCounter;

    trait PairStatsTracker extends TrackedIntStatistics[K2] { 
      protected def k1: K1;
      statistics += { (k2: K2, oldV: Int, newV: Int) =>
        outer.updateStatistics( (k1,k2), oldV, newV); 
      }
    }


  }

  trait BareCounterFactory[K1,K2] extends CounterFactory[K1,K2] {outer: BasePairedIntCounter[K1,K2] =>
    protected def mkIntCounter(k1:K1): IntCounter = new TIntCounter(k1);

    protected type SelfType[T1,T2] = BasePairedIntCounter[T1,T2];
    type IntCounter = TIntCounter;
    protected def mkPairCounter[T1,T2] : SelfType[T1,T2] = new BasePairedIntCounter[T1,T2] with BareCounterFactory[T1,T2];

    class TIntCounter(protected val k1: K1) 
        extends BaseIntCounter[K2] with PairStatsTracker {
      def copy = new TIntCounter(k1);
    }
  }

  trait TotaledCounterFactory[K1,K2] extends CounterFactory[K1,K2] {
    outer: BasePairedIntCounter[K1,K2] =>

     protected type SelfType[T1,T2] = PairedIntCounter[T1,T2];
     protected def mkPairCounter[T1,T2] : SelfType[T1,T2] = new PairedIntCounter[T1,T2];
     def mkIntCounter(k1: K1) = new TIntCounter(k1);
     type IntCounter = TIntCounter;


     class TIntCounter(protected val k1: K1) 
        extends BaseIntCounter[K2] with PairStatsTracker with TrackedIntStatistics.Total[K2] {
        def copy = new TIntCounter(k1);
     }
  }
}
