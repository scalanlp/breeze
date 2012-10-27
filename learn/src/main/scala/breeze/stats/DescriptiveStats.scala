package breeze.stats

import util.Sorting
;

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


/**
* Provides utilities for descriptive statistics, like the mean and variance.
*/
object DescriptiveStats {
  /**
  * Returns the mean and variance of an iterator in a pair.
  */
  def meanAndVariance[/*@specialized(Double, Float)*/ T ](it: TraversableOnce[T])(implicit frac: Fractional[T]) = {
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
  def mean[/*@specialized(Double, Float)*/ T](it : TraversableOnce[T])(implicit frac: Fractional[T]) = {
    val (sum,n) = accumulateAndCount(it);
    frac.div(sum , frac.fromInt(n));
  }

  def variance[/*@specialized(Double, Float)*/ T](it : TraversableOnce[T])(implicit n: Fractional[T]) = meanAndVariance(it)._2

  /**
   * Return the total sum and the number of T's
   */
  def accumulateAndCount[@specialized T](it : TraversableOnce[T])(implicit n: Numeric[T]) = it.foldLeft( (n.zero,0) ) { (tup,d) =>
    import n.mkNumericOps;
    (tup._1 + d, tup._2 + 1);
  }

  /**
   * returns the estimate of it at p * it.size, where p in [0,1]
   */
  def percentile(it: TraversableOnce[Double], p: Double) = {
    if(p > 1 || p < 0) throw new IllegalArgumentException("p must be in [0,1]")
    val arr = it.toArray
    Sorting.quickSort(arr)
    // +1 so that the .5 == mean for even number of elements.
    val f = (arr.length + 1) * p
    val i = f.toInt
    if(i == 0) arr.head
    else if (i >= arr.length) arr.last
    else {
      arr(i-1) + (f - i) * (arr(i) - arr(i-1))
    }
  }

}
