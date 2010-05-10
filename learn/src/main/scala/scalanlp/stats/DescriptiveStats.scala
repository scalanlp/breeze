package scalanlp.stats;

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


import scalala.tensor.counters._;
import Counters._
import scalanlp.util.Iterators;

/**
* Provides utilities for descriptive statistics, like the mean and variance.
*/
object DescriptiveStats {
  /**
  * Returns the mean and variance of an iterator in a pair.
  */
  def meanAndVariance[@specialized(Double, Float) T ](it :Iterator[T])(implicit frac: Fractional[T]) = {
    import frac.mkNumericOps;
    val (mu,s,n) = it.foldLeft( (frac.zero,frac.zero,0)) { (acc,y) =>
      val (oldMu,oldVar,n) = acc;
      val i = n+1;
      val d = y - oldMu;
      val mu = oldMu + frac.one/frac.fromInt(i) * d;
      val s = oldVar + frac.fromInt(i-1) / frac.fromInt(i) * d *d;
      (mu,s,i);
    }
    (mu,s/frac.fromInt(n-1));
  }

  /**
  * Returns the mean of the sequence of numbers
  */
  def mean[@specialized(Double, Float) T](it : Iterator[T])(implicit frac: Fractional[T]) = {
    val (sum,n) = accumulateAndCount(it);
    frac.div(sum , frac.fromInt(n));
  }

  def variance[@specialized(Double, Float) T](it : Iterator[T])(implicit n: Fractional[T]) = meanAndVariance(it)._2

  /**
   * Return the total sum and the number of T's
   */
  def accumulateAndCount[@specialized T](it : Iterator[T])(implicit n: Numeric[T]) = it.foldLeft( (n.zero,0) ) { (tup,d) =>
    import n.mkNumericOps;
    (tup._1 + d, tup._2 + 1);
  }

}
