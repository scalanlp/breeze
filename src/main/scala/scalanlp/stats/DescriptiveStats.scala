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


import scalanlp.counters._;
import Counters._
import scalanlp.util.Iterators;

/**
* Provides utilities for descriptive statistics, like the mean and variance.
*/
object DescriptiveStats {
  def meanAndVariance(it :Iterator[Double]) = {
    val (mu,s,n) = it.foldLeft( (0.0,0.0,0)) { (acc,y) =>
      val (oldMu,oldVar,n) = acc;
      val i = n+1;
      val d = y - oldMu;
      val mu = oldMu + 1.0/i * d;
      val s = oldVar + (i-1.0) / i * d *d;
      (mu,s,i);
    }
    (mu,s/(n-1));
  }

  def meanAndVariance[T](it : Iterator[T])(implicit f: T=>Double) = it.map(f);

  def mean(it :Iterator[Double]):Double = {
    val (sum,n) = Iterators.accumulateAndCount(it);
    sum / n;
  }

  def mean[T](it : Iterator[T])(implicit f: T=>Double): Double = mean(it.map(f));
  def mean[T <% Double](it : Iterable[T]):Double = mean(it.iterator);
  
  def mean[T <% Double](c : DoubleCounter[T]) :Double = mean(c map { (k,v) => k*v}) / c.total;

  def variance[T](it : Iterator[T])(implicit f: T=>Double): Double = meanAndVariance(it.map(f))._2
  def variance[T<%Double](it : Iterable[T]): Double = variance(it.iterator);
}
