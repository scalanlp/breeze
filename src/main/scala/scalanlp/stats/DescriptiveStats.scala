package scalanlp.stats;

import scalanlp.counters._;
import util.Iterators;

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
  def mean[T <% Double](it : Iterable[T]):Double = mean(it.elements);
  
  def mean[T <% Double](c : DoubleCounter[T]) :Double = mean(c map { case(k,v) => k*v}) / c.total;
  def mean[T <% Double](c : IntCounter[T]) :Double = mean(c map { case(k,v) => k*v}) / c.total;

  def variance[T](it : Iterator[T])(implicit f: T=>Double): Double = meanAndVariance(it.map(f))._2
  def variance[T<%Double](it : Iterable[T]): Double = variance(it.elements);
}
