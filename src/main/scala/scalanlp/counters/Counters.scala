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
import scalala.tensor.operators._;
import TensorShapes._;
import scalala.collection._;
import it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap;

import scalanlp.util._;

object Counters extends DoubleCounterFactory with IntCounterFactory {
  class DefaultDoubleCounter[T] extends
    AbstractDoubleCounter[T] with TrackedStatistics.Total[T]
    with TensorSelfOp[T,DefaultDoubleCounter[T],Shape1Col];

  class DefaultIntCounter[T] extends 
    AbstractIntCounter[T] with TrackedIntStatistics.Total[T];

  class DefaultPairedDoubleCounter[T1,T2] extends 
    AbstractPairedDoubleCounter[T1,T2] with TrackedStatistics.Total[(T1,T2)];

  protected abstract class DefaultInternalDoubleCounter[T1,T2] extends 
    DefaultDoubleCounter[T2] with PairStatsTracker[T1,T2];

  class DefaultPairedIntCounter[T1,T2] extends 
    AbstractPairedIntCounter[T1,T2] with TrackedIntStatistics.Total[(T1,T2)];

  protected abstract class DefaultInternalIntCounter[T1,T2] extends 
    DefaultIntCounter[T2] with PairIntStatsTracker[T1,T2];

  type DoubleCounter[T] = DefaultDoubleCounter[T];
  type IntCounter[T] = DefaultIntCounter[T];
  type PairedDoubleCounter[T1,T2] = DefaultPairedDoubleCounter[T1,T2];
  protected type InternalDoubleCounter[T1,T2] = DefaultInternalDoubleCounter[T1,T2];
  type PairedIntCounter[T1,T2] = DefaultPairedIntCounter[T1,T2];
  protected type InternalIntCounter[T1,T2] = DefaultInternalIntCounter[T1,T2];

  protected def mkDoubleCounter[T] = {
    val c = new DefaultDoubleCounter[T];
    c;
  }

  protected def mkIntCounter[T] = {
    val c = new DefaultIntCounter[T];
    c;
  }

  protected def mkPairedDoubleCounter[T1,T2]: PairedDoubleCounter[T1,T2] = {
    new DefaultPairedDoubleCounter[T1,T2];
  }

  protected def mkDoubleCounterFor[T1,T2](key: T1, pc: PairedDoubleCounter[T1,T2])
      : InternalDoubleCounter[T1,T2] = {
    new DefaultInternalDoubleCounter[T1,T2] { 
      def outer = pc;
      def k1 = key;
    }
  }

  protected def mkPairedIntCounter[T1,T2]: PairedIntCounter[T1,T2] = {
    new DefaultPairedIntCounter[T1,T2];
  }

  protected def mkIntCounterFor[T1,T2](key: T1, pc: PairedIntCounter[T1,T2])
      : InternalIntCounter[T1,T2] = {
    new DefaultInternalIntCounter[T1,T2] { 
      def outer = pc;
      def k1 = key;
    }
  }

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
    * Create a new IntCounter.
    */
    def apply[T]():IntCounter[T] = mkIntCounter[T];
  }

  object PairedDoubleCounter {
    /**
    * Create a new PairedDoubleCounter;
    */
    def apply[T1,T2]():PairedDoubleCounter[T1,T2] = new PairedDoubleCounter[T1,T2];
  }

  object PairedIntCounter {
    /**
    * Create a new PairedIntCounter;
    */
    def apply[T1,T2]():PairedIntCounter[T1,T2] = new PairedIntCounter[T1,T2];
  }

}


