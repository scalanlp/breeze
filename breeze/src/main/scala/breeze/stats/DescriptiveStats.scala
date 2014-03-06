package breeze.stats

import util.Sorting


/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
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
  * Returns the mean, variance and count of an iterator in a pair.
  */
  def meanAndVariance[/*@specialized(Double, Float)*/ T ](it: TraversableOnce[T])(implicit frac: Fractional[T]) = {
    import frac.mkNumericOps
    val (mu,s,n) = it.foldLeft( (frac.zero,frac.zero,0)) { (acc,y) =>
      val (oldMu,oldVar,n) = acc
      val i = n+1
      val d = y - oldMu
      val mu = oldMu + frac.one/frac.fromInt(i) * d
      val s = oldVar + frac.fromInt(i-1) / frac.fromInt(i) * d *d
      (mu,s,i)
    }
    (mu,if(n==1) frac.zero else s/frac.fromInt(n-1),n)
  }

  /**
  * Returns the mean of the sequence of numbers
  */
  def mean[/*@specialized(Double, Float)*/ T](it : TraversableOnce[T])(implicit frac: Fractional[T]) = {
    val (sum,n) = accumulateAndCount(it)
    frac.div(sum , frac.fromInt(n))
  }

  def variance[/*@specialized(Double, Float)*/ T](it : TraversableOnce[T])(implicit n: Fractional[T]) = meanAndVariance(it)._2

  /**
   * Return the total sum and the number of T's
   */
  def accumulateAndCount[@specialized T](it : TraversableOnce[T])(implicit n: Numeric[T]) = it.foldLeft( (n.zero,0) ) { (tup,d) =>
    import n.mkNumericOps
    (tup._1 + d, tup._2 + 1)
  }

  /**
   * returns the estimate of the data at p * it.size, where p in [0,1]
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
  
  /**
   * Returns both means and covariance between two vectors. Single pass algorithm.
   * <p>
   * Note:
   * Will happily compute covariance between vectors of different lengths
   * by truncating the longer vector.
   * </p>
   */
  
  def meanAndCov[T](it1 : TraversableOnce[T], it2 : TraversableOnce[T])(implicit frac: Fractional[T]) = {
    implicit def t(it:TraversableOnce[T]) = it.toIterable //convert to an iterable for zip operation
    import frac.mkNumericOps
    //mu1(n-1), mu2(n-1), Cov(n-1), n-1
    val (mu1,mu2,c,n) = (it1,it2).zipped.foldLeft( (frac.zero,frac.zero,frac.zero,frac.zero) ) {
      (acc,y) => val(oldMu1,oldMu2,oldC,oldN) = acc
      val newN = oldN + frac.fromInt(1)
      val newMu1 = oldMu1 + ((y._1 - oldMu1) / newN)
      val newMu2 = oldMu2 + ((y._2 - oldMu2) / newN)
      val newC = oldC + ((y._1 - oldMu1)*(y._2 - newMu2))//compute covariance in single pass
      (newMu1,newMu2,newC,newN)
    }
    if(n==1) (mu1,mu2,0) else (mu1,mu2,c/(n-frac.fromInt(1)))
  }

  /**
   * Returns covariance between two vectors.
   * <p>
   * Note:
   * Will happily compute covariance between vectors of different lengths
   * by truncating the longer vector.
   * </p>
   */
  
  def cov[T](it1 : Iterable[T], it2 : Iterable[T])(implicit n: Fractional[T]) = {
    meanAndCov(it1,it2)._3
  }
  
}
