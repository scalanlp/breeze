package breeze.math
/*
 Copyright 2012 David Hall

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
import breeze.linalg.operators._
import breeze.storage.DefaultArrayValue
import scala.reflect.ClassTag
import breeze.linalg.norm
import spire.algebra._
import spire.implicits._

/**
 * Immutable complex number representation backed by doubles
 * for the real and imaginary parts.
 *
 * Integration with `scala.math.Numeric` and `scala.math.Fractional` is
 * provided.
 *
 * @author dramage, lancelet
 */
case class Complex(real : Double, imag : Double) {
  override def toString = real + " + " + imag + "i"

  /** Redundant accessor method, placed for transparent interlink with MATLAB/Mathematica.
    */
  def re() = real
  /** Redundant accessor method, placed for transparent interlink with MATLAB/Mathematica.
    */
  def im() = imag

  def +(that : Complex) =
    Complex(this.real + that.real, this.imag + that.imag)

  def +(that : Int) =
    Complex(this.real + that, this.imag)

  def +(that : Long) =
    Complex(this.real + that, this.imag)

  def +(that : Float) =
    Complex(this.real + that, this.imag)

  def +(that : Double) =
    Complex(this.real + that, this.imag)

  def -(that : Complex) =
    Complex(this.real - that.real, this.imag - that.imag)

  def -(that : Int) =
    Complex(this.real - that, this.imag)

  def -(that : Long) =
    Complex(this.real - that, this.imag)

  def -(that : Float) =
    Complex(this.real - that, this.imag)

  def -(that : Double) =
    Complex(this.real - that, this.imag)

  def *(that : Complex) =
    Complex(this.real * that.real - this.imag * that.imag,
            this.real * that.imag + this.imag * that.real)

  def *(that : Int) =
    Complex(this.real * that, this.imag * that)

  def *(that : Long) =
    Complex(this.real * that, this.imag * that)

  def *(that : Float) =
    Complex(this.real * that, this.imag * that)

  def *(that : Double) =
    Complex(this.real * that, this.imag * that)

  def /(that : Complex) = {
    val denom = that.real * that.real + that.imag * that.imag
    Complex((this.real * that.real + this.imag * that.imag) / denom,
            (this.imag * that.real - this.real * that.imag) / denom)
  }

  def /(that : Int) =
    Complex(this.real / that, this.imag / that)

  def /(that : Long) =
    Complex(this.real / that, this.imag / that)

  def /(that : Float) =
    Complex(this.real / that, this.imag / that)

  def /(that : Double) =
    Complex(this.real / that, this.imag / that)

  def unary_- =
    Complex(-real, -imag)

  def abs =
    math.sqrt(real*real + imag*imag)

  def conjugate =
    Complex(real, -imag)

  def log =
    Complex(math.log(abs), math.atan2(imag, real))

  def exp = {
    val expreal = math.exp(real)
    Complex(expreal * math.cos(imag), expreal * math.sin(imag))
  }

  def pow(b: Double): Complex = pow(Complex(b, 0))

  def pow(b: Complex): Complex = {
    if (b == Complex.zero) Complex.one
    else if (this == Complex.zero) {
      if (b.imag != 0.0 || b.real < 0.0) Complex.nan
      else Complex.zero
    }
    else {
      val c = log * b
      val expReal = math.exp(c.real)
      Complex(expReal * math.cos(c.imag), expReal * math.sin(c.imag))
    }
  }

  override def equals(that : Any) = that match {
    case that : Complex => this.real == that.real && this.imag == that.imag
    case real : Double => this.real == real && this.imag == 0
    case real : Int => this.real == real && this.imag == 0
    case real : Short => this.real == real && this.imag == 0
    case real : Long => this.real == real && this.imag == 0
    case real : Float => this.real == real && this.imag == 0
    case _ => false
  }

  def abs2: Double = real*real+imag*imag
}

object Complex { outer =>

  /** Constant Complex(0,0). */
  val zero = new Complex(0,0)

  /** Constant Complex(1,0). */
  val one = new Complex(1,0)

  /** Constant Complex(NaN, NaN). */
  val nan = new Complex(Double.NaN, Double.NaN)

  /** Constant Complex(0,1). */
  val i = new Complex(0,1)

  //
  // scalar
  //

  implicit object scalar extends Field[Complex] {
    def zero = outer.zero

    def one = outer.one

    def nan = outer.nan

    def ==(a : Complex, b : Complex) = a == b

    def !=(a : Complex, b : Complex) = a != b

    def >(a : Complex, b : Complex) =
      (a.real > b.real || (a.real == b.real && a.imag > b.imag))

    def >=(a : Complex, b : Complex) =
      (a.real >= b.real || (a.real == b.real && a.imag >= b.imag))

    def <(a : Complex, b : Complex) =
      (a.real < b.real || (a.real == b.real && a.imag < b.imag))

    def <=(a : Complex, b : Complex) =
      (a.real <= b.real || (a.real == b.real && a.imag <= b.imag))

    def plus(a : Complex, b : Complex) = a + b

    def subtract(a : Complex, b : Complex) = a - b

    def times(a : Complex, b : Complex) = a * b

    def div(a : Complex, b : Complex) = a / b
    def quot(a : Complex, b : Complex) = a / b

    def norm(a : Complex) = a.abs

    def toDouble(a : Complex) =
      throw new UnsupportedOperationException("Cannot automatically convert complex numbers to doubles")

    def isNaN(a : Complex) =
      a.real.isNaN || a.imag.isNaN

    val manifest = implicitly[ClassTag[Complex]]

    val defaultArrayValue = DefaultArrayValue(Complex(0, 0))

    def negate(x: breeze.math.Complex): breeze.math.Complex = Complex(x.real, x.imag)
    //These two are necessary to implement for spire. They will go away when we replace Complex with Spire's complex.
    def gcd(a: breeze.math.Complex,b: breeze.math.Complex): breeze.math.Complex = ??? //Spire WTF is GCD of complex?
    def mod(a: breeze.math.Complex,b: breeze.math.Complex): breeze.math.Complex = ??? //Spire WTF is GCD of complex?
  }

  implicit val complexNorm: norm.Impl[Complex, Double] = new norm.Impl[Complex, Double] {
    def apply(v1: Complex): Double = v1.abs
  }

  implicit object ComplexDefaultArrayValue extends DefaultArrayValue[Complex] {
    val value = Complex(0, 0)
  }

  //
  // neg
  //

  implicit object Neg extends OpNeg.Impl[Complex, Complex]
    { def apply(v : Complex) = -v}

  //
  // add
  //

  implicit object AddCC extends OpAdd.Impl2[Complex, Complex, Complex]
  { def apply(a : Complex, b : Complex) = a + b}

  implicit object AddIC extends OpAdd.Impl2[Int, Complex, Complex]
  { def apply(a : Int, b : Complex) = a + b}

  implicit object AddLC extends OpAdd.Impl2[Long, Complex, Complex]
  { def apply(a : Long, b : Complex) = b + a}

  implicit object AddFC extends OpAdd.Impl2[Float, Complex, Complex]
  { def apply(a : Float, b : Complex) = b + a}

  implicit object AddDC extends OpAdd.Impl2[Double, Complex, Complex]
  { def apply(a : Double, b : Complex) = a + b}

  implicit object AddCI extends OpAdd.Impl2[Complex, Int, Complex]
  { def apply(a : Complex, b : Int) = a + b}

  implicit object AddCL extends OpAdd.Impl2[Complex, Long, Complex]
  { def apply(a : Complex, b : Long) = a + b}

  implicit object AddCF extends OpAdd.Impl2[Complex, Float, Complex]
  { def apply(a : Complex, b : Float) = a + b}

  implicit object AddCD extends OpAdd.Impl2[Complex, Double, Complex]
  { def apply(a : Complex, b : Double) = a + b}

  //
  // sub
  //

  implicit object SubCC extends OpSub.Impl2[Complex, Complex, Complex]
  { def apply(a : Complex, b : Complex) = a-b}

  implicit object SubIC extends OpSub.Impl2[Int, Complex, Complex]
  { def apply(a : Int, b : Complex) = Complex(a-b.real, -1*b.imag)}

  implicit object SubLC extends OpSub.Impl2[Long, Complex, Complex]
  { def apply(a : Long, b : Complex) = Complex(a-b.real, -1*b.imag) }

  implicit object SubFC extends OpSub.Impl2[Float, Complex, Complex]
  { def apply(a : Float, b : Complex) = Complex(a-b.real, -1*b.imag)}

  implicit object SubDC extends OpSub.Impl2[Double, Complex, Complex]
  { def apply(a : Double, b : Complex) = Complex(a-b.real, -1*b.imag) }

  implicit object SubCI extends OpSub.Impl2[Complex, Int, Complex]
  { def apply(a : Complex, b : Int) = Complex(a.real - b, a.imag)}

  implicit object SubCL extends OpSub.Impl2[Complex, Long, Complex]
  { def apply(a : Complex, b : Long) = Complex(a.real-b, a.imag)}

  implicit object SubCF extends OpSub.Impl2[Complex, Float, Complex]
  { def apply(a : Complex, b : Float) = Complex(a.real-b, a.imag)}

  implicit object SubCD extends OpSub.Impl2[Complex, Double, Complex]
  { def apply(a : Complex, b : Double) = Complex(a.real-b, a.imag)}

  //
  // mul
  //

  implicit object MulCC extends OpMulMatrix.Impl2[Complex, Complex, Complex]
  { def apply(a : Complex, b : Complex) = a * b}

  implicit object MulIC extends OpMulMatrix.Impl2[Int, Complex, Complex]
  { def apply(a : Int, b : Complex) = a * b}

  implicit object MulLC extends OpMulMatrix.Impl2[Long, Complex, Complex]
  { def apply(a : Long, b : Complex) = Complex(b.real*a, b.imag*a)}

  implicit object MulFC extends OpMulMatrix.Impl2[Float, Complex, Complex]
  { def apply(a : Float, b : Complex) = Complex(b.real*a, b.imag*a)}

  implicit object MulDC extends OpMulMatrix.Impl2[Double, Complex, Complex]
  { def apply(a : Double, b : Complex) = Complex(b.real*a, b.imag*a)}

  implicit object MulCI extends OpMulMatrix.Impl2[Complex, Int, Complex]
  { def apply(a : Complex, b : Int) = Complex(a.real*b, a.imag*b)}

  implicit object MulCL extends OpMulMatrix.Impl2[Complex, Long, Complex]
  { def apply(a : Complex, b : Long) = Complex(a.real*b, a.imag*b)}

  implicit object MulCF extends OpMulMatrix.Impl2[Complex, Float, Complex]
  { def apply(a : Complex, b : Float) = Complex(a.real*b, a.imag*b)}

  implicit object MulCD extends OpMulMatrix.Impl2[Complex, Double, Complex]
  { def apply(a : Complex, b : Double) = Complex(a.real*b, a.imag*b)}

  //
  // div
  //

  implicit object DivCC extends OpDiv.Impl2[Complex, Complex, Complex]
  { def apply(a : Complex, b : Complex) = a / b}

  implicit object DivIC extends OpDiv.Impl2[Int, Complex, Complex]
  { def apply(a : Int, b : Complex) = Complex(a*b.real,-1*a*b.imag)/b.abs2}

  implicit object DivLC extends OpDiv.Impl2[Long, Complex, Complex]
  { def apply(a : Long, b : Complex) = Complex(a*b.real,-1*a*b.imag)/b.abs2}

  implicit object DivFC extends OpDiv.Impl2[Float, Complex, Complex]
  { def apply(a : Float, b : Complex) = Complex(a*b.real,-1*a*b.imag)/b.abs2}

  implicit object DivDC extends OpDiv.Impl2[Double, Complex, Complex]
  { def apply(a : Double, b : Complex) = Complex(a*b.real,-1*a*b.imag)/b.abs2 }

  implicit object DivCI extends OpDiv.Impl2[Complex, Int, Complex]
  { def apply(a : Complex, b : Int) = Complex(a.real/b, a.imag/b)}

  implicit object DivCL extends OpDiv.Impl2[Complex, Long, Complex]
  { def apply(a : Complex, b : Long) = Complex(a.real/b, a.imag/b)}

  implicit object DivCF extends OpDiv.Impl2[Complex, Float, Complex]
  { def apply(a: Complex, b : Float) = Complex(a.real/b, a.imag/b)}

  implicit object DivCD extends OpDiv.Impl2[Complex, Double, Complex]
  { def apply(a : Complex, b : Double) = Complex(a.real/b, a.imag/b)}

  //
  // scala.math.Numeric and scala.math.Fractional
  //
  // TODO: Implement scala.math.Integral trait, if this is ever required
  //       for some reason.

  /** `Complex` as `scala.math.Numeric` trait.
    * Conversions to `Int`, `Long`, `Float` and `Double` are only performed
    * if the imaginary component of the complex number is exactly 0. */
  trait ComplexIsConflicted extends Numeric[Complex] {
    def plus(x: Complex, y: Complex): Complex = x + y
    def minus(x: Complex, y: Complex): Complex = x - y
    def times(x: Complex, y: Complex): Complex = x * y
    def negate(x: Complex): Complex = -x
    def fromInt(x: Int): Complex = Complex(x, 0)
    def toInt(x: Complex): Int = strictlyReal(x).toInt
    def toLong(x: Complex): Long = strictlyReal(x).toLong
    def toFloat(x: Complex): Float = strictlyReal(x).toFloat
    def toDouble(x: Complex): Double = strictlyReal(x)

    /** Checks that a `Complex` number is strictly real, and returns the real
      * component. */
    private def strictlyReal(x: Complex): Double = {
      require(x.imag == 0.0)  // only proceed if x.imag is *exactly* zero
      x.real
    }
  }
  /** `Complex` as `scala.math.Fractional` trait. */
  trait ComplexIsFractional extends ComplexIsConflicted
		            with Fractional[Complex]
  {
    def div(x: Complex, y: Complex): Complex = x / y
  }
  /** Ordering for complex numbers: orders lexicographically first
    * on the real, then on the imaginary part of the number. */
  trait ComplexOrdering extends Ordering[Complex] {
    override def compare(a : Complex, b : Complex) = {
      if (a.real < b.real) -1
      else if (a.real > b.real) 1
      else if (a.imag < b.imag) -1
      else if (a.imag > b.imag) 1
      else 0
    }
  }
  /** Implicit object providing `scala.math.Fractional` capabilities.
    * Although complex numbers have no natural ordering, some kind of
    * `Ordering` is required because `Numeric` extends `Ordering`.  Hence,
    * an ordering based upon the real then imaginary components is used. */
  implicit object ComplexIsFractional extends ComplexIsFractional
                                      with ComplexOrdering

  implicit object logComplexImpl extends breeze.numerics.log.Impl[Complex, Complex] { def apply(v: Complex) = v.log}
  implicit object expComplexImpl extends breeze.numerics.exp.Impl[Complex, Complex] { def apply(v: Complex) = v.exp}
  implicit object absComplexImpl extends breeze.numerics.abs.Impl[Complex, Double] { def apply(v: Complex) = v.abs}
  implicit object powComplexDoubleImpl extends breeze.numerics.pow.Impl2[Complex, Double, Complex] { def apply(v: Complex, d: Double) = v.pow(d) }
  implicit object powComplexComplexImpl extends breeze.numerics.pow.Impl2[Complex, Complex, Complex] { def apply(v: Complex, d: Complex) = v.pow(d) }

//  implicit def canMapValues[Tag, T, U](implicit impl: UImpl[Tag, Complex, Complex], canMapValues: CanMapValues[T, Complex, Complex, U]): UImpl[Tag, T, U] = {
//    new UImpl[Tag, T, U] {
//      def apply(v: T): U = canMapValues.map(v, impl.apply)
//    }
//  }

}
