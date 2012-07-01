package breeze.numerics

import breeze.linalg._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalacheck.{Prop, Arbitrary}

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

    (breeze.linalg.softmax(s,1,2,(s.length-1)/2, { (_) => true}) should be (mlog(3) plusOrMinus 1e-10))
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

  test("incomplete gamma") {
    import breeze.numerics.{lgamma=>lg}
    lg(3,4) should be (0.4212028764812177 plusOrMinus 1E-8)
    lg(3,1) should be (-1.828821079471455 plusOrMinus 1E-8)

  }

}
