package breeze.numerics

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

import breeze.linalg._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith
import org.scalacheck.{Prop, Arbitrary}
import breeze.math.Complex

@RunWith(classOf[JUnitRunner])
class NumericsTest extends FunSuite with Checkers with Matchers {

  test("softmax") {
    import math.{log=>mlog}
    import breeze.linalg.softmax
    (softmax(mlog(5.0), mlog(2)) should be (mlog(7) +- 1e-10))
    (softmax(mlog(2), mlog(5)) should be (mlog(7) +- 1e-10))
    (softmax(Double.NegativeInfinity, mlog(5)) should be (mlog(5) +- 1e-10))
    (softmax(mlog(5), Double.NegativeInfinity) should be (mlog(5) +- 1e-10))
    (softmax(Double.NegativeInfinity, Double.NegativeInfinity) should be (Double.NegativeInfinity))

    (softmax(Array(mlog(1), mlog(2), mlog(3))) should be (mlog(6) +- 1e-10))
    (softmax(Array(mlog(1), mlog(2), Double.NegativeInfinity)) should be (mlog(3) +- (1e-10)))

    val s = log1p(Array.tabulate(5)(_.toDouble))
    (softmax(s) should be (mlog(15) +- 1e-10))
    (softmax(Double.NegativeInfinity +: s) should be (mlog(15) +- 1e-10))
    (softmax(s :+ Double.NegativeInfinity) should be (mlog(15) +- 1e-10))

    (softmax(DenseVector(s)) should be (mlog(15) +- 1e-10))
    (softmax(DenseVector(s)(0 until s.length-1)) should be (mlog(10) +- 1e-10))
  }

  test("logDiff") {
    import breeze.linalg.logDiff
    (logDiff(log(5), log(2)) should be (log(3) +- 1e-10))
    (logDiff(log(5), log(5)) should be (Double.NegativeInfinity))

    evaluating {
      logDiff(log(5), log(6))
    } should produce[IllegalArgumentException]
  }

  import Arbitrary._

  implicit def ae(x: Double) = new {
    def =~=(y: Double) = breeze.numerics.closeTo(x, y, 1E-6)
  }

  // TODO 2.9 filter out Double.MaxValue.
  test("softmax is approximately associative") {
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
      Seq(a,b,c).exists(x => x > 1E300 || x < -1E300) ||
      softmax(a,softmax(b,c)) =~= softmax(softmax(a,b),c)
    })
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
      Seq(a,b,c).exists(x => x > 1E300 || x < -1E300) ||
      softmax(a,softmax(b,c)) =~= softmax(Array(a,b,c))
    })
  }

  test("sum distributes over softmax") {
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
      Seq(a,b,c).exists(x => x > 1E300 || x < -1E300) ||
      (a + softmax(b,c)) =~= (softmax(a + b,a+c))
    })
  }

  test("exp(digamma(x)) ≈ x - .5, x >= 10") {
    check(Prop.forAll { (a: Double) =>
      a.abs < 10 || a.abs > Double.MaxValue / 2 || exp(breeze.numerics.digamma(a.abs)) =~= (a.abs - .5)
    })
  }

  test("lgamma") {
    import breeze.numerics.{lgamma=>lg}
    lg(10) should be (12.8018274801 +- 1E-8)
  }

  test("lbeta") {
    assert(exp(breeze.numerics.lbeta(breeze.linalg.DenseVector(1.0, 2.0))) === 0.5)
  }

  test("incomplete gamma") {
    import breeze.numerics.{lgamma=>lg}
    import breeze.numerics.gammp
    lg(3.0,4.0) should be (0.4212028764812177 +- 1E-8)
    lg(3.0,1.0) should be (-1.828821079471455 +- 1E-8)
    assert(lg(3.0,DenseVector(4.0, 1.0))  === DenseVector(lg(3.0, 4.0), lg(3.0, 1.0)))
    assert(lg(DenseVector(3.0, 3.0),4.0)  === DenseVector(lg(3.0, 4.0), lg(3.0, 4.0)))
    assert(lg(DenseVector(3.0, 3.0),DenseVector(4.0, 1.0))  === DenseVector(lg(3.0, 4.0), lg(3.0, 1.0)))
    gammp(3.0, 1.0) should be (0.08030139707139419 +- 1E-8)
    gammp(3.0, 4.0) should be (0.7618966944464557 +- 1E-8)
    gammp(3.0, 10.0) should be (0.9972306042844884 +- 1E-8)
  }

  test("erf") {
    import breeze.numerics.{erf,erfi}
    erf(3.0) should be (.9999779095030014 +- 1E-8)
    erf(-3.0) should be (-.9999779095030014 +- 1E-8)
    erf(1E-4) should be (0.00011283791633342489 +- 1E-8)
    erfi(3.0) should be (1629.994622601567 +- 1E-4)
    erfi(-3.0) should be (-1629.994622601567 +- 1E-4)
    erf(1E-4) should be (0.00011283791708567767 +- 1E-8)
  }

  test("basic ufunc tests") {
    import breeze.numerics._
    exp(DenseVector(0.0))
    exp(DenseVector(0.0f))
    log(DenseVector(Complex.zero))
    log(SparseVector(Complex.zero))
    log(SparseVector(0.0))
    exp(SparseVector(0.0f))
  }

  test("in place works for exp") {
    import breeze.numerics._
    val v = DenseVector.rand(20)
    val expv = exp(v)
    exp.inPlace(v)
    assert(v === expv)

    val m = DenseMatrix.rand(2,2)
    val mexp = exp(m)
    exp.inPlace(m)
    assert(m === mexp)

    val sv = SparseVector.tabulate(3)(_.toDouble)
    val svexp = exp(sv)
    exp.inPlace(sv)
    assert(sv === svexp)
  }

  test("isOdd/isEven") {
    assert(isOdd(1), "1 should be odd!")
    assert(isEven(0.0), "0.0 should be even!")
    assert(isEven(DenseVector(2.0f, 1.5f, -3.0f)) == DenseVector(true, false, false) , "an array of floats")

  }

  test("sinc"){
    val testThreshold = 1.0E-15
    assert( abs(sinc(1d) - 0.8414709848078965) < testThreshold)
    assert( abs(sinc(1f) - 0.8414709848078965) < testThreshold*1.0E8)
    assert( sinc(0d) == 1d )

    val testDV = DenseVector( -10d, -7d, -4d, -1d )
    assert( norm( sinc(testDV) - DenseVector(-0.05440211108893698, 0.09385522838839844, -0.18920062382698205,0.8414709848078965) ) < testThreshold)
  }

  test("sincpi"){
    val testThreshold = 1.0E-15
    assert( abs(sincpi(1d) - 3.898171832519376E-17) < testThreshold)
    assert( abs(sincpi(1f) - 3.898171832519376E-17) < testThreshold*1.0E8)
    assert( sincpi(0d) == 1d )

    val testDV = DenseVector( -3d, -2.5, -2d, -1.5 )
    assert( norm( sincpi(testDV) - DenseVector(3.898171832519376E-17, 0.127323954473516, -3.898171832519376E-17, -0.212206590789194 ) ) < testThreshold)
  }

  test("nextPower"){
    assert( nextExponent10(0.05d) == -1d )
    //assert( nextExponent10(0d) == Double.NegativeInfinity )
    assert( nextExponent10(15d) == 2d )
    assert( nextPower10(0.05d) == 0.1d )
    //assert( nextPower10(0d) == 1d )
    assert( nextPower10(15d) == 100d )

    assert( nextExponent2(0.2) == -2d )
    //assert( nextExponent2(0d) == Double.NegativeInfinity )
    assert( nextExponent2(15d) == 4d )
    assert( nextPower2(0.2d) == 0.25d )
    //assert( nextPower2(0d) == 1d )
    assert( nextPower2(15d) == 16d )
  }

  test("log"){
    assert( log(2d, 4d) == 2d )
    assert( log(3d, 81d) == 4d )
  }
}
