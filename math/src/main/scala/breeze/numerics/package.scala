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

import breeze.generic.{MappingUFunc, UFunc}
import breeze.math.Semiring
import scala.math._
import org.apache.commons.math3.special.{Gamma => G, Erf}
import breeze.linalg.support.CanTraverseValues
import CanTraverseValues.ValuesVisitor
import org.apache.commons.math3.util.FastMath
import breeze.macros.expand

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

  object expm1 extends UFunc with MappingUFunc {
    implicit object expm1DoubleImpl extends Impl[Double, Double] { def apply(v: Double) = java.lang.Math.expm1(v)}
    implicit object expm1FloatImpl extends Impl[Float, Float] { def apply(v: Float) = java.lang.Math.expm1(v).toFloat}
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

    implicit object powIntIntImpl extends Impl2[Int,Int,Int] {
      def apply(v: Int, v2: Int) = IntMath.ipow(v,v2)
    }

    implicit object powIntDoubleImpl extends Impl2[Int,Double,Double] {
      def apply(v: Int, v2: Double) = java.lang.Math.pow(v,v2)
    }
  }



  // Logarithms, etc

  private val log2D = m.log(2d)
  private val log10D = m.log(10d)

  object log extends UFunc with MappingUFunc {
    //ToDo: Clarify in documentation that log(b, x) is the log of x in base b (instead of log b in base x)
    //ToDo???: extend to negative logs (but return type Double/Complex dependent on input???)
    implicit object logIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.log(v.toDouble)}
    implicit object logDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.log(v)}
    implicit object logFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.log(v).toFloat}
    implicit object logBDoubleImpl extends Impl2[Double, Double, Double] { def apply(base: Double, v: Double) = m.log(v)/m.log(base)}
    implicit object logBFloatImpl extends Impl2[Float, Float, Float] { def apply(base: Float, v: Float) = (m.log(v)/m.log(base)).toFloat}
  }

  object log2 extends UFunc with MappingUFunc {
    implicit object log2IntImpl extends Impl[Int, Double] { def apply(v: Int) = m.log(v.toDouble)/log2D}
    implicit object log2DoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.log(v)/log2D}
    implicit object log2FloatImpl extends Impl[Float, Float] { def apply(v: Float) = (m.log(v)/log2D).toFloat}
  }

  object log10 extends UFunc with MappingUFunc {
    implicit object log10IntImpl extends Impl[Int, Double] { def apply(v: Int) = m.log10(v.toDouble)}
    implicit object log10DoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.log10(v)}
    implicit object log10FloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.log10(v).toFloat}
  }

  object log1p extends UFunc with MappingUFunc {
    implicit object log1pIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.log1p(v)}
    implicit object log1pDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.log1p(v)}
    implicit object log1pFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.log1p(v).toFloat}
  }


  object nextExponent extends UFunc with MappingUFunc {
    implicit object nextExponentIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.ceil(m.log(v.toDouble)) }
    implicit object nextExponentDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.ceil(m.log(v)) }
    implicit object nextExponentFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.ceil(m.log(v)).toFloat }
    implicit object nextExponentIntImpl2 extends Impl2[Int, Int, Double] { def apply(base: Int, v: Int) = m.ceil(m.log(v.toDouble)/ m.log(base.toDouble)) }
    implicit object nextExponentDoubleImpl2 extends Impl2[Double, Double, Double] { def apply(base: Double, v: Double) = m.ceil(m.log(v)/ m.log(base)) }
    implicit object nextExponentFloatImpl2 extends Impl2[Float, Float, Float] { def apply(base: Float, v: Float) = m.ceil(m.log(v)/ m.log(base)).toFloat }
  }

  object nextExponent2 extends UFunc with MappingUFunc {
    implicit object nextExponent2IntImpl extends Impl[Int, Double] { def apply(v: Int) = m.ceil(m.log(v.toDouble)/ log2D) }
    implicit object nextExponent2DoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.ceil(m.log(v)/ log2D) }
    implicit object nextExponent2FloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.ceil(m.log(v)/ log2D).toFloat }
  }

  object nextExponent10 extends UFunc with MappingUFunc {
    implicit object nextExponent10IntImpl extends Impl[Int, Double] { def apply(v: Int) = m.ceil(m.log(v.toDouble)/log10D) }
    implicit object nextExponent10DoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.ceil(m.log(v)/log10D) }
    implicit object nextExponent10FloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.ceil(m.log(v)/log10D).toFloat }
  }

  object nextPower extends UFunc with MappingUFunc {
    implicit object nextPowerIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.exp( nextExponent(v.toDouble) ) }
    implicit object nextPowerDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.exp( nextExponent(v) ) }
    implicit object nextPowerFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.exp( nextExponent(v) ).toFloat }
    implicit object nextPowerIntImpl2 extends Impl2[Int, Int, Double] { def apply(base: Int, v: Int) = m.pow( base.toDouble, nextExponent(v.toDouble) ) }
    implicit object nextPowerDoubleImpl2 extends Impl2[Double, Double, Double] { def apply(base: Double, v: Double) = m.pow( base, nextExponent(v) ) }
    implicit object nextPowerFloatImpl2 extends Impl2[Float, Float, Float] { def apply(base: Float, v: Float) = m.pow( base, nextExponent(v) ).toFloat }
  }

  object nextPower2 extends UFunc with MappingUFunc {
    implicit object nextPower2IntImpl extends Impl[Int, Double] { def apply(v: Int) = m.pow(2d, nextExponent2(v.toDouble)) }
    implicit object nextPower2DoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.pow(2d, nextExponent2(v)) }
    implicit object nextPower2FloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.pow(2d, nextExponent2(v.toDouble)).toFloat }
  }

  object nextPower10 extends UFunc with MappingUFunc {
    implicit object nextPower10IntImpl extends Impl[Int, Double] { def apply(v: Int) = m.pow(10d, nextExponent10(v.toDouble)) }
    implicit object nextPower10DoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.pow(10d, nextExponent10(v)) }
    implicit object nextPower10FloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.pow(10d, nextExponent10(v.toDouble)).toFloat }
  }





  object sqrt extends UFunc with MappingUFunc {
    implicit object sqrtDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.sqrt(v)}
    implicit object sqrtFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.sqrt(v).toFloat}
    implicit object sqrtIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.sqrt(v.toDouble) }
    implicit object sqrtLongImpl extends Impl[Long, Double] { def apply(v: Long) = m.sqrt(v.toDouble) }
  }

  object cbrt extends UFunc with MappingUFunc {
    implicit object cbrtIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.cbrt(v.toDouble)}
    implicit object cbrtDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.cbrt(v)}
    implicit object cbrtFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.cbrt(v).toFloat}
  }

  object sin extends UFunc with MappingUFunc {
    implicit object sinIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.sin(v.toDouble)}
    implicit object sinDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.sin(v)}
    implicit object sinFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.sin(v).toFloat}
  }

  /**The sine cardinal (sinc) function, as defined by sinc(0)=1, sinc(n != 0)=sin(x)/x.
   * Note that this differs from some signal analysis conventions, where sinc(n != 0)
   * is defined by sin(Pi*x)/(Pi*x). This variant is provided for convenience as
   * [[breeze.numerics.sincpi]]. <b><i>Use it instead when translating from numpy.sinc.</i></b>.
   */
  object sinc extends UFunc with MappingUFunc {
    implicit object sincIntImpl extends Impl[Int, Double] {
      def apply(v: Int) = if(v == 0) 1d else m.sin(v.toDouble)/v.toDouble
    }
    implicit object sincDoubleImpl extends Impl[Double, Double] {
      def apply(v: Double) = if(v == 0) 1d else m.sin(v)/v
    }
    implicit object sincFloatImpl extends Impl[Float, Float] {
      def apply(v: Float) =  if(v == 0) 1f else m.sin(v).toFloat/v
    }
  }

  /**The pi-normalized sine cardinal (sinc) function, as defined by sinc(0)=1, sinc(n != 0)=sin(Pi*x)/(Pi*x).
    * See also [[breeze.numerics.sinc]].
    */
  object sincpi extends UFunc with MappingUFunc {
    implicit object sincpiIntImpl extends Impl[Int, Double] {
      def apply(v: Int) = if(v == 0) 1d else {val temp = v.toDouble * m.Pi;  m.sin(temp)/temp}
    }
    implicit object sincpiDoubleImpl extends Impl[Double, Double] {
      def apply(v: Double) = if(v == 0) 1d else {val temp = v * m.Pi;  m.sin(temp)/temp}
    }
    implicit object sincpiFloatImpl extends Impl[Float, Float] {
      def apply(v: Float) =  if(v == 0) 1f else  {val temp = v * m.Pi;  (m.sin(temp)/temp).toFloat}
    }
  }

  object cos extends UFunc with MappingUFunc {
    implicit object cosIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.cos(v.toDouble)}
    implicit object cosDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.cos(v)}
    implicit object cosFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.cos(v).toFloat}
  }

  object tan extends UFunc with MappingUFunc {
    implicit object tanIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.tan(v.toDouble)}
    implicit object tanDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.tan(v)}
    implicit object tanFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.tan(v).toFloat}
  }

  object sinh extends UFunc with MappingUFunc {
    implicit object sinIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.sinh(v.toDouble)}
    implicit object sinDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.sinh(v)}
    implicit object sinFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.sinh(v).toFloat}
  }

  object cosh extends UFunc with MappingUFunc {
    implicit object cosIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.cosh(v.toDouble)}
    implicit object cosDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.cosh(v)}
    implicit object cosFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.cosh(v).toFloat}
  }

  object tanh extends UFunc with MappingUFunc {
    implicit object tanIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.tanh(v.toDouble)}
    implicit object tanDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.tanh(v)}
    implicit object tanFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.tanh(v).toFloat}
  }

  object sech extends UFunc with MappingUFunc {
    implicit object tanIntImpl extends Impl[Int, Double] { def apply(v: Int) = 1.0/m.cosh(v.toDouble)}
    implicit object tanDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = 1.0/m.cosh(v)}
    implicit object tanFloatImpl extends Impl[Float, Float] { def apply(v: Float) = 1.0f/m.cosh(v).toFloat}
  }

  object asin extends UFunc with MappingUFunc {
    implicit object asinIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.asin(v.toDouble)}
    implicit object asinDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.asin(v)}
    implicit object asinFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.asin(v).toFloat}
  }

  object acos extends UFunc with MappingUFunc {
    implicit object acosIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.acos(v.toDouble)}
    implicit object acosDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.acos(v)}
    implicit object acosFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.acos(v).toFloat}
  }

  object atan extends UFunc with MappingUFunc {
    implicit object atanIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.atan(v.toDouble)}
    implicit object atanDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.atan(v)}
    implicit object atanFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.atan(v).toFloat}
  }


  object atan2 extends UFunc with MappingUFunc {
    implicit object atan2IntImpl extends Impl2[Int, Int, Double] { def apply(v: Int, v2: Int) = m.atan2(v.toDouble, v2.toDouble)}
    implicit object atan2DoubleImpl extends Impl2[Double, Double, Double] { def apply(v: Double, v2: Double) = m.atan2(v, v2)}
    implicit object atan2FloatImpl extends Impl2[Float, Float, Float] { def apply(v: Float, v2: Float) = m.atan2(v, v2).toFloat}
  }

  object asinh extends UFunc with MappingUFunc {
    implicit object asinIntImpl extends Impl[Int, Double] { def apply(v: Int) = FastMath.asinh(v.toDouble)}
    implicit object asinDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = FastMath.asinh(v)}
    implicit object asinFloatImpl extends Impl[Float, Float] { def apply(v: Float) = FastMath.asinh(v).toFloat}
  }

  object acosh extends UFunc with MappingUFunc {
    implicit object acosIntImpl extends Impl[Int, Double] { def apply(v: Int) = FastMath.acosh(v.toDouble)}
    implicit object acosDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = FastMath.acosh(v)}
    implicit object acosFloatImpl extends Impl[Float, Float] { def apply(v: Float) = FastMath.acosh(v).toFloat}
  }

  object atanh extends UFunc with MappingUFunc {
    implicit object atanIntImpl extends Impl[Int, Double] { def apply(v: Int) = FastMath.atanh(v.toDouble)}
    implicit object atanDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = FastMath.atanh(v)}
    implicit object atanFloatImpl extends Impl[Float, Float] { def apply(v: Float) = FastMath.atanh(v).toFloat}
  }

  object toDegrees extends UFunc with MappingUFunc {
    implicit object toDegreesIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.toDegrees(v.toDouble)}
    implicit object toDegreesDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.toDegrees(v)}
    implicit object toDegreesFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.toDegrees(v).toFloat}
  }

  object toRadians extends UFunc with MappingUFunc {
    implicit object toRadiansIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.toRadians(v.toDouble)}
    implicit object toRadiansDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.toRadians(v)}
    implicit object toRadiansFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.toRadians(v).toFloat}
  }

  object floor extends UFunc with MappingUFunc {
    implicit object floorIntImpl extends Impl[Int, Int] { def apply(v: Int) = v}
    implicit object floorDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.floor(v)}
    implicit object floorFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.floor(v).toFloat}
  }

  object ceil extends UFunc with MappingUFunc {
    implicit object ceilIntImpl extends Impl[Int, Int] { def apply(v: Int) = v}
    implicit object ceilDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.ceil(v)}
    implicit object ceilFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.ceil(v).toFloat}
  }

  object round extends UFunc with MappingUFunc {
    implicit object roundIntImpl extends Impl[Int, Int] { def apply(v: Int) = v}
    implicit object roundDoubleImpl extends Impl[Double, Long] { def apply(v: Double) = m.round(v)}
    implicit object roundFloatImpl extends Impl[Float, Int] { def apply(v: Float) = m.round(v)}
  }

  object rint extends UFunc with MappingUFunc {
    implicit object rintIntImpl extends Impl[Int, Int] { def apply(v: Int) = v}
    implicit object rintDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.rint(v)}
    implicit object rintFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.rint(v).toFloat}
  }

  object signum extends UFunc with MappingUFunc {
    implicit object signumIntImpl extends Impl[Int, Double] { def apply(v: Int) = m.signum(v.toDouble)}
    implicit object signumDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.signum(v)}
    implicit object signumFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.signum(v)}
  }

  object abs extends UFunc with MappingUFunc {
    implicit object absDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = m.abs(v)}
    implicit object absFloatImpl extends Impl[Float, Float] { def apply(v: Float) = m.abs(v)}
    implicit object absIntImpl extends Impl[Int, Int] { def apply(v: Int) = m.abs(v)}
    implicit object absLongImpl extends Impl[Long, Long] { def apply(v: Long) = m.abs(v)}
  }

  /** Whether a number is odd. For Double and Float, isOdd also implies that the number is an integer,
    * and therefore does not necessarily equal !isEven for fractional input.
    */
  object isOdd extends UFunc with MappingUFunc {
    @expand
    @expand.valify
    implicit def isOddImpl[@expand.args(Int, Double, Float, Long) T]: Impl[T, Boolean] = {
      new Impl[T, Boolean] {
        def apply(v: T) = {v % 2 == 1}
      }
    }
  }

  /** Whether a number is even. For Double and Float, isEven also implies that the number is an integer,
    * and therefore does not necessarily equal !isOdd for fractional input.
    */
  object isEven extends UFunc with MappingUFunc {
    @expand
    @expand.valify
    implicit def isEvenImpl[@expand.args(Int, Double, Float, Long) T]: Impl[T, Boolean] = {
      new Impl[T, Boolean] {
        def apply(v: T) = {v % 2 == 0}
      }
    }
  }

  val inf, Inf = Double.PositiveInfinity
  val nan, NaN = Double.NaN

  object isNonfinite extends UFunc with MappingUFunc {
    @expand
    @expand.valify
    implicit def isNonfiniteImpl[@expand.args(Double, Float) T]: Impl[T, Boolean] = {
      new Impl[T, Boolean] {
        override def apply(v: T): Boolean = {
          // TODO: only in Java 8
//          !java.lang.Double.isFinite(v)
          !isFinite(v)
        }
      }
    }
  }

  object isFinite extends UFunc with MappingUFunc {
    @expand
    @expand.valify
    implicit def isFiniteImpl[@expand.args(Double, Float) T]: Impl[T, Boolean] = {
      new Impl[T, Boolean] {
        override def apply(v: T): Boolean = {
          m.abs(v) <= Double.MaxValue
        }
      }
    }
  }

  /**
   * Computes the log of the gamma function. The two parameter version
   * is the log Incomplete gamma function = \log \int_0x \exp(-t)pow(t,a-1) dt
   *
   * @return an approximation of the log of the Gamma function of x.
   */
  object lgamma extends UFunc with MappingUFunc {
    implicit object lgammaImplInt extends Impl[Int, Double] {
      def apply(v: Int): Double = if(v == 0) Double.PositiveInfinity else G.logGamma(v.toDouble)
    }
    implicit object lgammaImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = if(v == 0.0) Double.PositiveInfinity else G.logGamma(v)
    }


    implicit object lgammaImplIntInt extends Impl2[Int, Int, Double] {
      def apply(a: Int, x: Int): Double = lgammaImplDoubleDouble(a.toDouble, x.toDouble)
    }

    /**
     * log Incomplete gamma function = \log \int_0x \exp(-t)pow(t,a-1) dt
     * May store lgamma(a) in lgam(0) if it's non-null and needs to be computed.
     * Based on NR
     */
    implicit object lgammaImplDoubleDouble extends Impl2[Double, Double, Double] {
      def apply(a: Double, x: Double): Double = {
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

          if (n == 100) throw new ArithmeticException("Convergence failed")
          else breeze.linalg.logDiff(gln, -x+a*m.log(x) + m.log(h))
        }
      }
    }
  }


  /**
   * The derivative of the log gamma function
   */
  object digamma extends UFunc with MappingUFunc {
    implicit object digammaImplInt extends Impl[Int, Double] {
      def apply(v: Int): Double = digammaImplDouble(v.toDouble)
    }
    implicit object digammaImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = G.digamma(v)
    }
  }


  /**
   * The second derivative of the log gamma function
   */
  object trigamma extends UFunc with MappingUFunc {
    implicit object trigammaImplInt extends Impl[Int, Double] {
      def apply(v: Int): Double = trigammaImplDouble(v.toDouble)
    }
    implicit object trigammaImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = G.trigamma(v)
    }
  }


  /**
   * Evaluates the log of the generalized beta function.
   *  \sum_a lgamma(c(a))- lgamma(c.sum)
   */
  object lbeta extends UFunc {
    implicit object impl2Double extends Impl2[Double, Double, Double] {
      def apply(v: Double, v2: Double): Double = {
        lgamma(v) + lgamma(v2) - lgamma(v + v2)
      }
    }

    implicit def reduceDouble[T](implicit iter: CanTraverseValues[T, Double]): Impl[T, Double] = new Impl[T, Double] {
      def apply(v: T): Double = {

        val visit = new ValuesVisitor[Double] {
          var sum = 0.0
          var lgSum = 0.0
          def visit(a: Double): Unit = {
            sum += a
            lgSum += lgamma(a)
          }

          def zeros(numZero: Int, zeroValue: Double): Unit = {
            sum += numZero * zeroValue
            lgSum += lgamma(zeroValue)
          }
        }

        iter.traverse(v, visit)

        visit.lgSum - lgamma(visit.sum)
      }

    }
  }


  /**
   * An approximation to the error function
   */
  object erf extends UFunc with MappingUFunc {
    implicit object erfImplInt extends Impl[Int, Double] {
      def apply(v: Int): Double = Erf.erf(v.toDouble)
    }
    implicit object erfImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = Erf.erf(v)
    }
  }



  /**
   * An approximation to the complementary error function: erfc(x) = 1 - erfc(x)
   */
  object erfc extends UFunc with MappingUFunc {
    implicit object erfcImplInt extends Impl[Int, Double] {
      def apply(v: Int): Double = Erf.erfc(v.toDouble)
    }
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
    implicit object erfiImplInt extends Impl[Int, Double] {
      def apply(v: Int): Double = erfiImplDouble(v.toDouble)
    }
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
    implicit object erfinvImplInt extends Impl[Int, Double] {
      def apply(v: Int): Double = Erf.erfInv(v.toDouble)
    }
    implicit object erfinvImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = Erf.erfInv(v)
    }

  }

  /**
   * Inverse erfc
   */
  object erfcinv extends UFunc with MappingUFunc {
    implicit object erfcinvImplInt extends Impl[Int, Double] {
      def apply(v: Int): Double = Erf.erfcInv(v.toDouble)
    }
    implicit object erfcinvImplDouble extends Impl[Double, Double] {
      def apply(v: Double): Double = Erf.erfcInv(v)
    }
  }

  /**
   * regularized incomplete gamma function  \int_0x \exp(-t)pow(t,a-1) dt / Gamma(a)
   * @see http://commons.apache.org/proper/commons-math/apidocs/org/apache/commons/math3/special/Gamma.html#regularizedGammaP(double, double)
   */
  object gammp extends UFunc with MappingUFunc {
    implicit object gammpImplIntInt extends Impl2[Int, Int, Double] {
      def apply(v1: Int, v2: Int): Double = G.regularizedGammaP(v1.toDouble, v2.toDouble)
    }
    implicit object gammpImplDoubleDouble extends Impl2[Double, Double, Double] {
      def apply(v1: Double, v2: Double): Double = G.regularizedGammaP(v1, v2)
    }
  }

  /**
   * regularized incomplete gamma function  \int_0x \exp(-t)pow(t,a-1) dt / Gamma(a)
   * @see http://commons.apache.org/proper/commons-math/apidocs/org/apache/commons/math3/special/Gamma.html#regularizedGammaP(double, double)
   */
  object gammq extends UFunc with MappingUFunc {
    implicit object gammqImplIntInt extends Impl2[Int, Int, Double] {
      def apply(v1: Int, v2: Int): Double = G.regularizedGammaP(v1.toDouble, v2.toDouble)
    }
    implicit object gammqImplDoubleDouble extends Impl2[Double, Double, Double] {
      def apply(v1: Double, v2: Double): Double = G.regularizedGammaP(v1, v2)
    }
  }

  /**
   * The sigmoid function: 1/(1 + exp(-x))
   *
   *
   */
  object sigmoid extends UFunc with MappingUFunc {
    implicit object sigmoidImplInt extends Impl[Int, Double] {
      def apply(x:Int) = 1d/(1d+scala.math.exp(-x.toDouble))
    }
    implicit object sigmoidImplDouble extends Impl[Double, Double] {
      def apply(x:Double) = 1d/(1d+scala.math.exp(-x))
    }

    implicit object sigmoidImplFloat extends Impl[Float, Float] {
      def apply(x:Float) = 1f/(1f+scala.math.exp(-x).toFloat)
    }
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
   * For non-boolean arguments, 1.0 iff b != 0, else 0.0
   */
  object I extends UFunc with MappingUFunc {
    implicit object iBoolImpl extends Impl[Boolean, Double] {
      def apply(b: Boolean) = if (b) 1.0 else 0.0
    }

    implicit def vImpl[V:Semiring]: Impl[V, Double] = new Impl[V, Double] {
      def apply(b: V) = if (b != implicitly[Semiring[V]].zero) 1.0 else 0.0
    }
  }


  /**
   * The indicator function in log space: 0.0 iff b else Double.NegativeInfinity
   */
  object logI extends UFunc with breeze.generic.MappingUFunc {
    implicit object logIBoolImpl extends Impl[Boolean, Double] {
      def apply(b: Boolean) = if (b) 0.0 else -inf
    }
  }

}

