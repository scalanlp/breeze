package scalanlp.math;

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

import counters._;

/**
* Provides some functions left out of java.lang.Math
* @author (dlwh)
*/
object Numerics {
  import Math._;
  /**
   * The standard digamma function.
   */
  def digamma(xx: Double) = {
    var x = xx;
    var r = 0.0;

    while(x<=5) {
      r -= 1/x;
      x += 1;
    }

    var f = 1./(x * x);
    var t = f*(-1/12.0 + 
            f*(1/120.0 +
            f*(-1/252.0 +
            f*(1/240.0 +
            f*(-1/132.0 +
            f*(691/32760.0 +
            f*(-1/12.0 +
            f*3617./8160.0)))))));
    r + log(x) - 0.5/x + t;
  }

  private val cof =  Array(76.18009172947146, -86.50532032941677,
    24.01409824083091,-1.231739572450155,
    0.1208650973866179e-2,-0.5395239384953e-5
  );
  
  /**
   * Evaluates the log of the generalized beta function.
   *  = \sum_a lgamma(c(a))- lgamma(c.total)
   */
  def lbeta[T](c: DoubleCounter[T]) = {
    c.values.foldLeft(-lgamma(c.total))( (acc,x)=> acc +lgamma(x));
  }

  /**
  * @return an approximation of the log of the Gamma function * of x.  Laczos Approximation
  * Reference: Numerical Recipes in C
  * http://www.library.cornell.edu/nr/cbookcpdf.html
  * www.cs.berkeley.edu/~milch/blog/versions/blog-0.1.3/blog/distrib 
  */ 
  def lgamma(x : Double) = {
    var y = x;
    var tmp = x + 5.5;
    tmp -= ((x + 0.5) * log(tmp));
    var ser = 1.000000000190015;
    var j = 0;
    while(j < 6) {
      y += 1;
      ser += (cof(j)/y);
      j +=1;
    }
    (-tmp + log(2.5066282746310005*ser / x));
  }
  
  /**
  * @return log(exp(a) + exp(b))
  */
  def logSum(a : Double, b : Double) = {
    if(a < b) b + log(1 + exp(a-b))
    else a + log(1+exp(b-a));    
  }

  /**
  * @return log(\sum exp(a_i))
  */
  def logSum(a: Double, b:Double, c: Double*):Double ={
    logSum(Array(a,b) ++ c);
  }

  /**
  * @return log(\sum exp(a_i))
  */
  def logSum(a:Seq[Double]):Double = {
    val iter = a.elements;
    if(a.length == 1) a(0)
    else if(a.length ==2) logSum(iter.next,iter.next);
    else {
      val m = a reduceLeft(_ max _);
      m + log(a.foldLeft(0.)( (a,b) => a+exp( b - m )))
    }
  }

  /*
  * @return log(exp(a) - exp(b))
  */
  def logDiff(a : Double, b : Double) = {
    if(a < b) b + log(exp(a-b) - 1)
    else a + log( 1- exp(b-a) );    
  }


}
