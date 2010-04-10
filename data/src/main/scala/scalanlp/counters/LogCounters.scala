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

/**
 * LogCounters are much like Counters, except they track
 * logTotal instead of total. LogTotal is also the softmax of
 * the elements in the counter. These are useful when you
 * need to deal with log-probabilities, which is frequent in NLP settings.
 *
 * @author dlwh
 */
object LogCounters extends DoubleCounterFactory {
  class LogDoubleCounter[T] extends AbstractDoubleCounter[T]
      with TrackedStatistics.LogTotal[T]
      with TensorSelfOp[T,LogDoubleCounter[T],Shape1Col] {
    default = Double.NegativeInfinity;
  }

  class LogPairedDoubleCounter[T1,T2] extends 
    AbstractPairedDoubleCounter[T1,T2] with TrackedStatistics.LogTotal[(T1,T2)]
    with TensorSelfOp[(T1,T2),LogPairedDoubleCounter[T1,T2],Shape2];

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
  def normalize[T](ctr: LogDoubleCounter[T]):Counters.DoubleCounter[T] = {
    val result = Counters.DoubleCounter[T]();

    for( (k,v) <- ctr) {
      result(k) = Math.exp(v - ctr.logTotal);
    }

    result;
  }

  /**
  * Returns a LogCounters.LogPairedDoubleCounter whose rows have
  * (approximately) logTotal log(1).
  */
  def logNormalize[T,U](ctr: Counters.PairedDoubleCounter[T,U]) = {
    val result = LogPairedDoubleCounter[T,U]();

    val logTotal = Math.log(ctr.total);
    for( (k1,c) <- ctr.rows;
         (k2,v) <- c ) {
      result(k1,k2) = Math.log(v) - logTotal;
    }

    result;
  }

  /**
  * Returns a LogCounters.LogPairedDoubleCounter whose rows have
  * (approximately) logTotal log(1).
  */
  def logNormalize[T,U](ctr: LogPairedDoubleCounter[T,U]) = {
    val result = LogPairedDoubleCounter[T,U]();

    for( (k,v) <- ctr.rows) {
      result(k) := v - ctr.logTotal;
    }

    result;
  }

  /**
  * Returns a LogCounters.LogPairedDoubleCounter whose rows have
  * (approximately) logTotal log(1).
  */
  def logNormalizeRows[T,U](ctr: Counters.PairedDoubleCounter[T,U]) = {
    val result = LogPairedDoubleCounter[T,U]();
    for( (k,v) <- ctr.rows) {
      result(k) := logNormalize(v);
    }

    result;
  }


  /**
  * Returns a LogCounters.LogPairedDoubleCounter whose rows have
  * (approximately) logTotal log(1).
  */
  def logNormalizeRows[T,U](ctr: LogPairedDoubleCounter[T,U]) = {
    val result = LogPairedDoubleCounter[T,U]();

    for( (k,v) <- ctr.rows) {
      result(k) := logNormalize(v);
    }

    result;
  }


  /**
  * Returns a LogCounters.LogDoubleCounter that has (approximately) total 1.
  * Each entry (k,v) has a new entry in the map (k,(v - logTotal))
  */
  def logNormalize[T](ctr: LogDoubleCounter[T]) = {
    val result = LogDoubleCounter[T]();

    for( (k,v) <- ctr) {
      assert(!(v - ctr.logTotal).isNaN,k + " " + v + " " + ctr.logTotal);
      result(k) = v - ctr.logTotal;
    }
    assert(result.logTotal < 1E-10);

    result;
  }


  /**
  * Returns a LogCounters.LogDoubleCounter that is just the log of these counts.
  */
  def log[T,U](ctr: Counters.PairedDoubleCounter[T,U]): LogPairedDoubleCounter[T,U] = {
    val result = LogPairedDoubleCounter[T,U]();

    for( (k,v) <- ctr) {
      result(k) = Math.log(v);
    }

    result
  }


  /**
  * Returns a Counters.PairedDoubleCounter that is just the exp of these log-counts.
  */
  def exp[T,U](ctr: LogPairedDoubleCounter[T,U]): Counters.PairedDoubleCounter[T,U] = {
    val result = Counters.PairedDoubleCounter[T,U]();

    for( (k,v) <- ctr) {
      result(k) = Math.exp(v);
    }

    result
  }


  /**
  * Returns a LogCounters.LogDoubleCounter that is just the log of these counts.
  */
  def log[T](ctr: Counters.DoubleCounter[T]): LogDoubleCounter[T] = {
    val result = LogDoubleCounter[T]();

    for( (k,v) <- ctr) {
      result(k) = Math.log(v);
    }

    result
  }



  /**
  * Returns a LogCounters.LogDoubleCounter that has (approximately) total 1.
  * Each entry (k,v) has a new entry in the map (k,(v - logTotal))
  */
  def logNormalize[T](ctr: Counters.DoubleCounter[T]): LogDoubleCounter[T] = {
    val result = LogDoubleCounter[T]();

    val logTotal = Math.log(ctr.total);
    for( (k,v) <- ctr) {
      result(k) = Math.log(v) - logTotal;
    }

    result;
  }

  /**
  * Returns a LogCounters.LogDoubleCounter that has (approximately) total 1.
  * Each entry (k,v) has a new entry in the map (k,(v - logTotal))
  */
  def logSum[T,U](ctr: LogPairedDoubleCounter[T,U], ctr2: LogPairedDoubleCounter[T,U]):LogPairedDoubleCounter[T,U] = {
    val result = LogPairedDoubleCounter[T,U]();

    for( k <- ctr.activeDomain ++ ctr2.activeDomain) {
      val res =  logSum(ctr(k),ctr2(k));
      if(res != Double.NegativeInfinity)
        result(k) = res
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

  /**
  * Returns a LogDoubleCounter over T1's, where the T1 counts are
  * the logTotals of their counters in the passsed-in counter.
  */
  def marginalize[T1,T2](ctr: LogPairedDoubleCounter[T1,T2]) = {
    val result = LogDoubleCounter[T1]();

    for( (k1,c) <- ctr.rows) {
      result(k1) = c.logTotal;
    }

    result;
  }

  /**
  * Returns KL(c1||c2)
  */
  def klDivergence[T1](c1: LogDoubleCounter[T1], c2: LogDoubleCounter[T1]) = {
    import scala.util.control.Breaks._;
    var result = 0.0;

    breakable {
      for( (k1,logV1) <- c1) {
        val logV2 = c2(k1);
        if(logV2 == Double.NegativeInfinity) {
          result = Double.PositiveInfinity;
          break;
        }
        result += Math.exp(logV1-c1.logTotal) * (logV1 - c1.logTotal - logV2 + c2.logTotal);  
      }
    }

    result;
  }


  /**
  * Sums together things in log space.
  * @return log(exp(a) + exp(b))
  */
  protected[counters] def logSum(a : Double, b : Double):Double = {
    if(a == Double.NegativeInfinity) b
    else if (b == Double.NegativeInfinity) a
    else if(a < b) b + Math.log(1 + Math.exp(a-b))
    else a + Math.log(1+Math.exp(b-a));    
  }
  
  /**
  * Sums together things in log space.
  * @return log(\sum exp(a_i))
  */
  protected[counters] def logSum(iter:Iterator[Double], max: Double):Double = {
    max + Math.log(iter.foldLeft(0.)( (a,b) => if(b == Double.NegativeInfinity) a else a+Math.exp( b - max )))
  }


}
