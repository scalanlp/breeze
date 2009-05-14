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
import it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap;

import scalanlp.util._;

/**
* Provides utilities for creating counters, once you specify a counter
* TDoubleCounter[T] that you would like to create, and mkDoubleCounter(set) for
* creating new ones.
* <p/>
* See object Counters for an example implementation.
*
* @author dlwh
*/
trait CounterFactory {
  /**
  * Abstract type that implementers must define. It must be a 
  * subtype of the AbstractDoubleCounter[T] trait.
  */
  type TDoubleCounter[T] <: AbstractDoubleCounter[T];

  /**
  * Abstract type that implementers must define. It must be a 
  * subtype of the AbstractDoubleCounter[T] trait.
  */
  type TIntCounter[T] <: AbstractIntCounter[T];

  /**
  * Given a domain (in the form of a mergeable set) create a new TDoubleCounter.
  */
  protected def mkDoubleCounter[T](set: MergeableSet[T]): TDoubleCounter[T];
  /**
  * Given a domain (in the form of a mergeable set) create a new TDoubleCounter.
  */
  protected def mkDoubleCounter[T]: TDoubleCounter[T] = mkDoubleCounter(EmptySet());

  /**
  * Given a domain (in the form of a mergeable set) create a new TIntCounter.
  */
  protected def mkIntCounter[T](set: MergeableSet[T]): TIntCounter[T];
  /**
  * Given a domain (in the form of a mergeable set) create a new TIntCounter.
  */
  protected def mkIntCounter[T]: TIntCounter[T] = mkIntCounter(EmptySet());


  /**
  * Trait that implementors of CounterFactory should implement.
  */
  abstract trait AbstractDoubleCounter[T] extends BaseDoubleCounter[T] {
    override def create[J](set: MergeableSet[J]) = mkDoubleCounter(set);
    // XXX TODO: should create a PairedDoubleCounter (of some type) for 2D domains.
    override def copy() = { 
      val c = create(this.activeDomain);
      c += this;
      c;
    }
  }


  /**
  * Trait that implementors of CounterFactory should implement.
  */
  abstract trait AbstractIntCounter[T] extends BaseIntCounter[T] {
    // XXX TODO: should create a PairedDoubleCounter (of some type) for 2D domains.
    def copy() = { 
      val c = mkIntCounter[T];
      c += this;
      c;
    }
  }


  /**
  * Return a new counter with counts equal to the number of 
  * times each x was seen.
  */ 
  def count[T](x: T, xs: T*):TIntCounter[T] = {
    val c = count(xs.elements);
    c.incrementCount(x,1);
    c;
  }

  /**
  * Return a new counter with counts equal to the number of 
  * times each x was seen.
  */ 
  def count[T](xs: Iterable[T]):TIntCounter[T] = count(xs.elements)

  /**
  * Return a new counter with counts equal to the number of 
  * times each x was seen.
  */ 
  def count[T](xs: Iterator[T]) = {
    val c = mkIntCounter[T];
    for(x <- xs) {
      c.incrementCount(x,1)
    }
    c
  }

  /**
  * Return a counter after adding in all the tuples.
  */ 
  def aggregate[T](xs: (T,Double)*):TDoubleCounter[T] = aggregate(xs.elements);

  /**
  * Return a counter after adding in all the tuples.
  */ 
  def aggregate[T](xs: Iterable[(T,Double)]):TDoubleCounter[T] = aggregate(xs.elements);

  /**
  * Return a counter after adding in all the tuples.
  */ 
  def aggregate[T](xs: Iterator[(T,Double)]) = {
    val c = mkDoubleCounter[T];
    c ++= xs;
    c;
  }

}
