package breeze

/*
 Copyright 2012 David Hall

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

import breeze.generic.{MappingUFunc, URFunc, UFunc}
import scala.math._
import org.apache.commons.math3.special.{Gamma => G, Erf}

/**
 * Contains several standard numerical functions as UFunc with MappingUFuncs,
 *
 * @author dlwh, afwlehmann
 */
package object numerics {


  import scala.{math=>m}

  // TODO: I should probably codegen this.

  object exp extends UFunc with MappingUFunc {
    implicit object expDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = java.lang.Math.exp(v)}
    implicit object expFloatImpl extends Impl[Float, Float] { def apply(v: Float) = java.lang.Math.exp(v).toFloat}
  }

  object pow extends UFunc with MappingUFunc {
    implicit object powDoubleDoubleImpl extends Impl2[Double, Double, Double] {
      def apply(v: Double, v2: Double) = java.lang.Math.pow(v, v2)
    }

    implicit object powFloatFloatImpl extends Impl2[Float, Float, Float] {
      def apply(v: Float, v2: Float) = java.lang.Math.pow(v, v2).toFloat
    }

    implicit object powDoubleIntImpl extends Impl2[Double, Int, Double] {
      def apply(v: Double, v2: Int) = java.lang.Math.pow(v, v2)
    }

    implicit object powFloatIntImpl extends Impl2[Float, Int, Float] {
      def apply(v: Float, v2: Int) = java.lang.Math.pow(v, v2).toFloat
    }
  }

  object log extends UFunc with MappingUFunc {
    implicit object logDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.log(v)}
    implicit object logFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.log(v).toFloat}
  }


  object log1p extends UFunc with MappingUFunc {
    implicit object log1pDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.log1p(v)}
    implicit object log1pFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.log1p(v).toFloat}
  }

  object sqrt extends UFunc with MappingUFunc {
    implicit object sqrtDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.sqrt(v)}
    implicit object sqrtFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.sqrt(v).toFloat}
  }

  object sin extends UFunc with MappingUFunc {
    implicit object sinDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.sin(v)}
    implicit object sinFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.sin(v).toFloat}
  }

  object cos extends UFunc with MappingUFunc {
    implicit object cosDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.cos(v)}
    implicit object cosFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.cos(v).toFloat}
  }

  object tan extends UFunc with MappingUFunc {
    implicit object tanDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.tan(v)}
    implicit object tanFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.tan(v).toFloat}
  }

  object asin extends UFunc with MappingUFunc {
    implicit object asinDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.asin(v)}
    implicit object asinFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.asin(v).toFloat}
  }

  object acos extends UFunc with MappingUFunc {
    implicit object acosDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.acos(v)}
    implicit object acosFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.acos(v).toFloat}
  }

  object atan extends UFunc with MappingUFunc {
    implicit object atanDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.atan(v)}
    implicit object atanFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.atan(v).toFloat}
  }

  object toDegrees extends UFunc with MappingUFunc {
    implicit object toDegreesDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.toDegrees(v)}
    implicit object toDegreesFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.toDegrees(v).toFloat}
  }

  object toRadians extends UFunc with MappingUFunc {
    implicit object toRadiansDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.toRadians(v)}
    implicit object toRadiansFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.toRadians(v).toFloat}
  }

  object floor extends UFunc with MappingUFunc {
    implicit object floorDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.floor(v)}
    implicit object floorFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.floor(v).toFloat}
  }

  object ceil extends UFunc with MappingUFunc {
    implicit object ceilDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.ceil(v)}
    implicit object ceilFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.ceil(v).toFloat}
  }

  object round extends UFunc with MappingUFunc {
    implicit object roundDoubleImpl extends Impl[Double, Long] { def apply(v: Double) = m.round(v)}
    implicit object roundFloatImpl extends Impl[Float, Int] { def apply(v: Float) = m.round(v)}
  }

  object rint extends UFunc with MappingUFunc {
    implicit object rintDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.rint(v)}
    implicit object rintFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.rint(v).toFloat}
  }

  object signum extends UFunc with MappingUFunc {
    implicit object signumDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.signum(v)}
    implicit object signumFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.signum(v)}
  }

  object abs extends UFunc with MappingUFunc {
    implicit object absDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.abs(v)}
    implicit object absFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.abs(v)}
  }

  val inf, Inf = Double.PositiveInfinity
  val nan, NaN = Double.NaN

  /**
   * Computes the log of the gamma function. The UFunc with MappingUFunc2 version (i.e. two parameter version)
   * is the log Incomplete gamma function = \log \int_0x \exp(-t)pow(t,a-1) dt
   *
   * @return an approximation of the log of the Gamma function of x.
   */
  object lgamma extends UFunc with MappingUFunc {
    implicit object lgammaImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = G.logGamma(v)
    }


    implicit object lgammaImplDoubleDouble extends Impl2[Double, Double, Double] {
      def apply(v1: Double, v2: Double): Double = _lgamma(v1, v2)
    }
  }


  /**
   * The derivative of the log gamma function
   */
  object digamma extends UFunc with MappingUFunc {
    implicit object digammaImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = G.digamma(v)
    }
  }


  /**
   * The second derivative of the log gamma function
   */
  object trigamma extends UFunc with MappingUFunc {
    implicit object trigammaImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = G.trigamma(v)
    }
  }


  /**
   * Evaluates the log of the generalized beta function.
   *  \sum_a lgamma(c(a))- lgamma(c.sum)
   */
  val lbeta:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]): Double = {
      var sum = 0.0
      var lgSum = 0.0
      for(v <- cc) {
        sum += v
        lgSum += lgamma(v:Double)
      }
      lgSum - lgamma(sum)
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean): Double = {
      var off = offset
      var sum = 0.0
      var lgSum = 0.0
      var i = 0
      while(i < length) {

        if(isUsed(off))   {
          sum += arr(off)
          lgSum += lgamma(arr(off))
        }

        i += 1
        off += stride
      }
      lgSum - lgamma(sum)
    }
  }


  /**
   * An approximation to the error function
   */
  object erf extends UFunc with MappingUFunc {
    implicit object erfImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = Erf.erf(v)
    }
  }



  /**
   * An approximation to the complementary error function: erfc(x) = 1 - erfc(x)
   */
  object erfc extends UFunc with MappingUFunc {
    implicit object erfcImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = Erf.erfc(v)
    }

  }


  /**
   * The imaginary error function for real argument x.
   *
   * Adapted from http://www.mathworks.com/matlabcentral/newsreader/view_thread/24120
   * verified against mathematica
   *
   * @return
   */
  object erfi extends UFunc with MappingUFunc {
    implicit object erfiImplDouble extends Impl[Double, Double] {
      def apply(x:Double):Double = {
        if(x < 0) -apply(-x)
        else { // taylor expansion
        var y = x
          val x2 = x * x
          var xx = x
          var f = 1.0
          var n  = 0
          while (n < 100) {
            n += 1
            f /= n
            xx *= x2
            val del =  f * xx/(2*n+1)
            if(del < 1E-8) n = 101
            y += del
          }
          y = y*2/m.sqrt(Pi)
          y
        }
      }
    }

  }


  /**
   * Inverse erf
   */
  object erfinv extends UFunc with MappingUFunc {
    implicit object erfinvImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = Erf.erfInv(v)
    }

  }

  /**
   * Inverse erfc
   */
  object erfcinv extends UFunc with MappingUFunc {
    implicit object erfcinvImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = Erf.erfcInv(v)
    }
  }

  /**
   * regularized incomplete gamma function  \int_0x \exp(-t)pow(t,a-1) dt / Gamma(a)
   * @see http://commons.apache.org/proper/commons-math/apidocs/org/apache/commons/math3/special/Gamma.html#regularizedGammaP(double, double)
   */
  object gammp extends UFunc with MappingUFunc {
    implicit object gammpImplDoubleDouble extends Impl2[Double, Double, Double] {
      def apply(v1: Double, v2: Double): Double = G.regularizedGammaP(v1, v2)
    }
  }

  /**
   * regularized incomplete gamma function  \int_0x \exp(-t)pow(t,a-1) dt / Gamma(a)
   * @see http://commons.apache.org/proper/commons-math/apidocs/org/apache/commons/math3/special/Gamma.html#regularizedGammaP(double, double)
   */
  object gammq extends UFunc with MappingUFunc {
    implicit object gammqImplDoubleDouble extends Impl2[Double, Double, Double] {
      def apply(v1: Double, v2: Double): Double = G.regularizedGammaP(v1, v2)
    }
  }




  /**
  * log Incomplete gamma function = \log \int_0x \exp(-t)pow(t,a-1) dt
  * May store lgamma(a) in lgam(0) if it's non-null and needs to be computed.
   * Based on NR
  */
  private def _lgamma(a: Double, x:Double, lgam: Array[Double] = null):Double = {
    if (x < 0.0 || a <= 0.0) throw new IllegalArgumentException()
    else if(x == 0) 0.0
    else if (x < a + 1.0) {
      var ap = a
      var del, sum = 1.0/a
      var n = 0
      var result = Double.NaN
      while(n < 100) {
        ap += 1
        del *= x/ap
        sum += del
        if (scala.math.abs(del) < scala.math.abs(sum)*1E-7) {
          result = -x+a*m.log(x) + m.log(sum)
          n = 100
        }
        n += 1
      }
      if(lgam != null) lgam(0) = Double.NaN
      if(result.isNaN) throw new ArithmeticException("Convergence failed")
      else result
    } else {
      val gln = lgamma(a)
      var b = x+1.0-a
      var c = 1.0/1.0e-30
      var d = 1.0/b
      var h = d
      var n = 0
      while(n < 100) {
        n += 1
        val an = -n*(n-a)
        b += 2.0
        d = an*d+b
        if (scala.math.abs(d) < 1E-30) d = 1E-30
        c = b+an/c
        if (scala.math.abs(c) < 1E-30) c = 1E-30
        d = 1.0/d
        val del = d*c
        h *= del
        if (scala.math.abs(del-1.0) < 1E-7) n = 101
      }

      if(lgam != null) lgam(0) = gln
      if (n == 100) throw new ArithmeticException("Convergence failed")
      else logDiff(gln, -x+a*m.log(x) + m.log(h))
    }
  }


  /**
  * Sums together things in log space.
  * @return log(exp(a) + exp(b))
  */
  def logSum(a: Double, b: Double) = {
    if (a.isNegInfinity) b
    else if (b.isNegInfinity) a
    else if (a < b) b + scala.math.log1p(m.exp(a - b))
    else a + scala.math.log1p(m.exp(b - a))
  }

  /**
  * Sums together things in log space.
  * @return log(\sum exp(a_i))
  */
  def logSum(a: Double, b: Double, c: Double*): Double = {
    if (c.length == 0)
      logSum(a, b)
    else
      logSum(logSum(a, b) +: c)
  }

  /**
  * Sums together things in log space.
  * @return log(\sum exp(a_i))
  */
  def logSum(iter: Iterator[Double], max: Double): Double = {
    require(iter.hasNext)
    if (max.isInfinite) {
      max
    } else {
      val aux = (0.0 /: iter) {
        (acc, x) => if (x.isNegInfinity) acc else acc + m.exp(x-max)
      }
      if (aux != 0)
        max + scala.math.log(aux)
      else
        max
    }
  }

  /**
  * Sums together things in log space.
  * @return log(\sum exp(a_i))
  */
  def logSum(a: Seq[Double]): Double = {
    a.length match {
      case 0 => Double.NegativeInfinity
      case 1 => a(0)
      case 2 => logSum(a(0), a(1))
      case _ => logSum(a.iterator, a reduceLeft (_ max _))
    }
  }

  /**
   * Sums together the first length elements in log space.
   * The length parameter is used to make things faster.
   *
   * This method needs to be fast. Don't scala-ify it.
   * @return log(\sum^length exp(a_i))
   */
  def logSum(a: Array[Double], length: Int):Double = {
    length match {
      case 0 => Double.NegativeInfinity
      case 1 => a(0)
      case 2 => logSum(a(0),a(1))
      case _ =>
        val m = max(a, length)
        if(m.isInfinite) m
        else {
          var i = 0
          var accum = 0.0
          while(i < length) {
            accum += scala.math.exp(a(i) - m)
            i += 1
          }
          m + scala.math.log(accum)
        }
    }
  }

  /** fast versions of max. Useful for the fast logsum. */
  def max(a: Array[Double], length: Int) = {
    var i = 1
    var max =  a(0)
    while(i < length) {
      if(a(i) > max) max = a(i)
      i += 1
    }
    max

  }

  /**
   * The sigmoid function: 1/(1 + exp(-x))
   *
   *
   */
  object sigmoid extends UFunc with MappingUFunc {
    implicit object sigmoidImplDouble extends Impl[Double, Double] {
      def apply(x:Double) = 1/(1+scala.math.exp(-x))
    }
  }

  /**
   * Takes the difference of two doubles in log space. Requires a &gt b.
   * Note that this only works if a and b are close in value. For a &gt;&gt; b,
   * this will almost certainly do nothing. (exp(30) - exp(1) \approx exp(30))
   *
   * @return log(exp(a) - exp(b))
   */
  def logDiff(a: Double, b: Double): Double = {
    require(a >= b)
    if (a > b) a + m.log(1.0 - m.exp(b-a))
    else Double.NegativeInfinity
  }

  /**
   * Computes the polynomial P(x) with coefficients given in the passed in array.
   * coefs(i) is the coef for the x_i term.
   */
  def polyval(coefs: Array[Double], x: Double) = {
    var i = coefs.length-1
    var p = coefs(i)
    while (i>0) {
      i -= 1
      p = p*x + coefs(i)
    }
    p
   }


  /**
   * closeTo for Doubles.
   */
  def closeTo(a: Double, b: Double, relDiff: Double = 1E-4) = {
    a == b || (scala.math.abs(a-b) < scala.math.max(scala.math.max(scala.math.abs(a),scala.math.abs(b)) ,1) * relDiff)
  }


  /**
   * The indicator function. 1.0 iff b, else 0.0
   */
  object I extends UFunc with MappingUFunc {
    implicit object iBoolImpl extends Impl[Boolean, Double] {
      def apply(b: Boolean) = if (b) 1.0 else 0.0
    }
  }


  /**
   * The indicator function in log space: 0.0 iff b else Double.NegativeInfinity
   */
  object logI extends UFunc with breeze.generic.MappingUFunc {
    implicit object logIBoolImpl extends Impl[Boolean, Double] {
      def apply(b: Boolean) = if (b) 0.0 else Double.NegativeInfinity
    }
  }

}

