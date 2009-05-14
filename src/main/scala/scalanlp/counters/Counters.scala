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


object Counters extends CounterFactory {
  // Various utility types you might want:

  /**
  * Abstract type representing the standard counter.
  */
  type DoubleCounter[T] = BaseDoubleCounter[T] with TrackedStatistics.Total[T]

  /**
  * Abstract type representing the standard counter.
  */
  type IntCounter[T] = BaseIntCounter[T] with TrackedIntStatistics.Total[T]


  class DefaultDoubleCounter[T] extends 
    AbstractDoubleCounter[T] with TrackedStatistics.Total[T];

  class DefaultIntCounter[T] extends 
    AbstractIntCounter[T] with TrackedIntStatistics.Total[T];

  type TDoubleCounter[T] = DefaultDoubleCounter[T];

  def mkDoubleCounter[T](set: MergeableSet[T]) = {
    val c = new DefaultDoubleCounter[T];
    c;
  }

  def mkIntCounter[T](set: MergeableSet[T]) = {
    val c = new DefaultIntCounter[T];
    c;
  }

  type TIntCounter[T] = DefaultIntCounter[T];

  /**
  * Object for easily creating "default" DoubleCounters
  */
  object DoubleCounter {
    /**
    * Create a new DoubleCounter .
    */
    def apply[T]():DoubleCounter[T] = mkDoubleCounter[T];
  }

  /**
  * Object for easily creating "default" IntCounters
  */
  object IntCounter {
    /**
    * Create a new DoubleCounter .
    */
    def apply[T]():IntCounter[T] = mkIntCounter[T];
  }
}
