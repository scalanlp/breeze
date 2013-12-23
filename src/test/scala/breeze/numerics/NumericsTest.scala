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
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalacheck.{Prop, Arbitrary}
import breeze.math.Complex

@RunWith(classOf[JUnitRunner])
class NumericsTest extends FunSuite with Checkers with ShouldMatchers {

  test("logSum") {
    import math.{log=>mlog}
    (breeze.numerics.logSum(mlog(5.0), mlog(2)) should be (mlog(7) plusOrMinus 1e-10))
    (breeze.numerics.logSum(mlog(2), mlog(5)) should be (mlog(7) plusOrMinus 1e-10))
    (breeze.numerics.logSum(Double.NegativeInfinity, mlog(5)) should be (mlog(5) plusOrMinus 1e-10))
    (breeze.numerics.logSum(mlog(5), Double.NegativeInfinity) should be (mlog(5) plusOrMinus 1e-10))
    (breeze.numerics.logSum(Double.NegativeInfinity, Double.NegativeInfinity) should be (Double.NegativeInfinity))

    (breeze.numerics.logSum(mlog(1), mlog(2), mlog(3)) should be (mlog(6) plusOrMinus 1e-10))
    (breeze.numerics.logSum(mlog(1), mlog(2), Double.NegativeInfinity) should be (mlog(3) plusOrMinus (1e-10)))

    val s = log1p(Array.tabulate(5)(_.toDouble))
    (breeze.numerics.logSum(s.iterator, s.max) should be (mlog(15) plusOrMinus 1e-10))
    (breeze.numerics.logSum(s) should be (mlog(15) plusOrMinus 1e-10))
    (breeze.numerics.logSum(Double.NegativeInfinity +: s) should be (mlog(15) plusOrMinus 1e-10))
    (breeze.numerics.logSum(s :+ Double.NegativeInfinity) should be (mlog(15) plusOrMinus 1e-10))

    (breeze.numerics.logSum(s,s.length) should be (mlog(15) plusOrMinus 1e-10))
    (breeze.numerics.logSum(s,s.length-1) should be (mlog(10) plusOrMinus 1e-10))
  }

  test("logDiff") {
    (logDiff(log(5), log(2)) should be (log(3) plusOrMinus 1e-10))
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
  test("logsumming is approximately associative") {
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
      Seq(a,b,c).exists(x => x > 1E300 || x < -1E300) ||
      breeze.numerics.logSum(a,breeze.numerics.logSum(b,c)) =~= breeze.numerics.logSum(breeze.numerics.logSum(a,b),c)
    })
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
      Seq(a,b,c).exists(x => x > 1E300 || x < -1E300) ||
      breeze.numerics.logSum(a,breeze.numerics.logSum(b,c)) =~= breeze.numerics.logSum(a,b,c)
    })
  }

  test("sum distributes over logsum") {
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
      Seq(a,b,c).exists(x => x > 1E300 || x < -1E300) ||
      (a + breeze.numerics.logSum(b,c)) =~= (breeze.numerics.logSum(a + b,a+c))
    })
  }

  test("exp(digamma(x)) ≈ x - .5, x >= 10") {
    check(Prop.forAll { (a: Double) =>
      a.abs < 10 || a.abs > Double.MaxValue / 2 || exp(breeze.numerics.digamma(a.abs)) =~= (a.abs - .5)
    })
  }

  test("lgamma") {
    import breeze.numerics.{lgamma=>lg}
    lg(10) should be (12.8018274801 plusOrMinus 1E-8)
  }

  test("lbeta") {
    assert(exp(breeze.numerics.lbeta(breeze.linalg.DenseVector(1.0, 2.0))) === 0.5)
  }

  test("incomplete gamma") {
    import breeze.numerics.{lgamma=>lg}
    import breeze.numerics.gammp
    lg(3.0,4.0) should be (0.4212028764812177 plusOrMinus 1E-8)
    lg(3.0,1.0) should be (-1.828821079471455 plusOrMinus 1E-8)
    assert(lg(3.0,DenseVector(4.0, 1.0))  === DenseVector(lg(3.0, 4.0), lg(3.0, 1.0)))
    assert(lg(DenseVector(3.0, 3.0),4.0)  === DenseVector(lg(3.0, 4.0), lg(3.0, 4.0)))
    assert(lg(DenseVector(3.0, 3.0),DenseVector(4.0, 1.0))  === DenseVector(lg(3.0, 4.0), lg(3.0, 1.0)))
    gammp(3.0, 1.0) should be (0.08030139707139419 plusOrMinus 1E-8)
    gammp(3.0, 4.0) should be (0.7618966944464557 plusOrMinus 1E-8)
    gammp(3.0, 10.0) should be (0.9972306042844884 plusOrMinus 1E-8)
  }

  test("erf") {
    import breeze.numerics.{erf,erfi}
    erf(3.0) should be (.9999779095030014 plusOrMinus 1E-8)
    erf(-3.0) should be (-.9999779095030014 plusOrMinus 1E-8)
    erf(1E-4) should be (0.00011283791633342489 plusOrMinus 1E-8)
    erfi(3.0) should be (1629.994622601567 plusOrMinus 1E-4)
    erfi(-3.0) should be (-1629.994622601567 plusOrMinus 1E-4)
    erf(1E-4) should be (0.00011283791708567767 plusOrMinus 1E-8)
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

}
