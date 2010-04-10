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

/**
 * Counters are a core abstraction in ScalaNLP, and can be thought of
 * as maps whose values are always doubles, and who maintain their total.
 * They also implement the Tensor interfaces from Scalala, so they can be
 * used as tensors. There are also IntCounters, which have a more limited
 * set of functionality and are not tensors.
 *
 * We provide two kinds of counters: DoubleCounters (Tensor1's) and PairedDoubleCounter (Tensor2's)
 *
 * @author dlwh
 */
object Counters extends DoubleCounterFactory with IntCounterFactory {
  class DefaultDoubleCounter[T] extends
    AbstractDoubleCounter[T] with TrackedStatistics.Total[T]
    with TensorSelfOp[T,DefaultDoubleCounter[T],Shape1Col];

  class DefaultIntCounter[T] extends 
    AbstractIntCounter[T] with TrackedIntStatistics.Total[T];

  class DefaultPairedDoubleCounter[T1,T2] extends 
    AbstractPairedDoubleCounter[T1,T2] with TrackedStatistics.Total[(T1,T2)]
    with TensorSelfOp[(T1,T2),DefaultPairedDoubleCounter[T1,T2],Shape2];

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
    def apply[T](pairs : (T,Double)*) = {
      val counter = mkDoubleCounter[T];
      for (pair <- pairs) counter(pair._1) = pair._2;
      counter;
    }
  }

  /**
  * Object for easily creating "default" IntCounters
  */
  object IntCounter {
    /**
    * Create a new IntCounter.
    */
    def apply[T]():IntCounter[T] = mkIntCounter[T];
    def apply[T](pairs : (T,Int)*) = {
      val counter = mkIntCounter[T];
      for (pair <- pairs) counter(pair._1) = pair._2;
      counter;
    }
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

  /**
  * Returns a LogDoubleCounter over T1's, where the T1 counts are
  * the logTotals of their counters in the passsed-in counter.
  */
  def marginalize[T1,T2](ctr: PairedDoubleCounter[T1,T2]) = {
    val result = DoubleCounter[T1]();

    for( (k1,c) <- ctr.rows) {
      result(k1) = c.total;
    }

    result;
  }


  /**
  * Returns a Counters.DoubleCounter that has (approximately) total 1.
  * Each entry (k,v) has a new entry in the map (k,v/total)
  */
  def normalize[T](ctr: DoubleCounter[T]):Counters.DoubleCounter[T] = {
    val result = Counters.DoubleCounter[T]();

    for( (k,v) <- ctr) {
      result(k) = v /ctr.total;
    }

    result;
  }


  
  /**
  * Returns a Counters.PairedDoubleCounter that has (approximately) total 1.
  * Each entry ( (k1,k2),v) has a new entry in the map (k,v/total)
  */
  def normalize[T1,T2](ctr: PairedDoubleCounter[T1,T2]) = {
    val result = Counters.PairedDoubleCounter[T1,T2]();

    for( ((k1,k2),v) <- ctr) {
      result(k1,k2) = v / ctr.total;
    }

    result;
  }
  
  import scalanlp.counters.LogCounters.LogPairedDoubleCounter;
  /** Returns a Counters.PairedDoubleCounter that has (approximately) total 1.
  * Each entry ( (k1,k2),v) has a new entry in the map (k,exp(v - logTotal))
  */
  def normalize[T1,T2](ctr: LogPairedDoubleCounter[T1,T2]) = {
    val result = Counters.PairedDoubleCounter[T1,T2]();

    for( ((k1,k2),v) <- ctr) {
      result(k1,k2) = Math.exp(v - ctr.logTotal);
    }

    result;
  }


}


