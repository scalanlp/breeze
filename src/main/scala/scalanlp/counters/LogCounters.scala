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
import scalala.tensor.operators._;
import TensorShapes._;
import scalala.tensor.sparse._;
import scalala.collection._;
import it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap;

import scalanlp.util._;

object LogCounters extends DoubleCounterFactory {
  class LogDoubleCounter[T] extends AbstractDoubleCounter[T]
      with TrackedStatistics.LogTotal[T] 
      with TensorSelfOp[T,LogDoubleCounter[T],Shape1Col];

  class LogPairedDoubleCounter[T1,T2] extends 
    AbstractPairedDoubleCounter[T1,T2] with TrackedStatistics.LogTotal[(T1,T2)];

  protected abstract class LogInternalDoubleCounter[T1,T2] extends 
    LogDoubleCounter[T2] with PairStatsTracker[T1,T2];

  type DoubleCounter[T] = LogDoubleCounter[T];
  type PairedDoubleCounter[T1,T2] = LogPairedDoubleCounter[T1,T2];
  protected type InternalDoubleCounter[T1,T2] = LogInternalDoubleCounter[T1,T2];

  protected def mkDoubleCounter[T] = {
    val c = new LogDoubleCounter[T];
    c;
  }


  protected def mkPairedDoubleCounter[T1,T2]: PairedDoubleCounter[T1,T2] = {
    new LogPairedDoubleCounter[T1,T2];
  }

  protected def mkDoubleCounterFor[T1,T2](key: T1, pc: PairedDoubleCounter[T1,T2])
      : InternalDoubleCounter[T1,T2] = {
    new LogInternalDoubleCounter[T1,T2] { 
      def outer = pc;
      def k1 = key;
    }
  }


  /**
  * Object for easily creating "default" DoubleCounters
  */
  object LogDoubleCounter {
    /**
    * Create a new DoubleCounter .
    */
    def apply[T]():DoubleCounter[T] = mkDoubleCounter[T];
  }

  object LogPairedDoubleCounter {
    /**
    * Create a new PairedDoubleCounter;
    */
    def apply[T1,T2]():PairedDoubleCounter[T1,T2] = mkPairedDoubleCounter[T1,T2];
  }

  /**
  * Returns a Counters.DoubleCounter that has (approximately) total 1.
  * Each entry (k,v) has a new entry in the map (k,exp(v - logTotal))
  */
  def normalize[T](ctr: LogDoubleCounter[T]) = {
    val result = Counters.DoubleCounter[T]();

    for( (k,v) <- ctr) {
      result(k) = Math.exp(v - ctr.logTotal);
    }

    result;
  }


  /**
  * Returns a LogCounters.LogDoubleCounter that has (approximately) total 1.
  * Each entry (k,v) has a new entry in the map (k,exp(v - logTotal))
  */
  def logNormalize[T](ctr: LogDoubleCounter[T]) = {
    val result = LogDoubleCounter[T]();

    for( (k,v) <- ctr) {
      result(k) = v - ctr.logTotal;
    }

    result;
  }




  /**
  * Returns a Counters.PairedDoubleCounter that has (approximately) total 1.
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
