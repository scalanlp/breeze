package breeze.numerics

@RunWith(classOf[JUnitRunner])
class NumericsTest extends FunSuite with Checkers with ShouldMatchers {

  test("logSum") {
    logSum(log(5), log(2)) should be(log(7) plusOrMinus 1e-10)
    logSum(log(2), log(5)) should be(log(7) plusOrMinus 1e-10)
    logSum(Double.NegativeInfinity, log(5)) should be(log(5) plusOrMinus 1e-10)
    logSum(log(5), Double.NegativeInfinity) should be(log(5) plusOrMinus 1e-10)
    logSum(Double.NegativeInfinity, Double.NegativeInfinity) should be(Double.NegativeInfinity)

    logSum(log(1), log(2), log(3)) should be(log(6) plusOrMinus 1e-10)
    logSum(log(1), log(2), Double.NegativeInfinity) should be(log(3) plusOrMinus (1e-10))

    val s = Array.tabulate[Double](5)(i => log1p(i))
    logSum(s.iterator, s.max) should be(log(15) plusOrMinus 1e-10)
    logSum(s) should be(log(15) plusOrMinus 1e-10)
    logSum(Double.NegativeInfinity +: s) should be(log(15) plusOrMinus 1e-10)
    logSum(s :+ Double.NegativeInfinity) should be(log(15) plusOrMinus 1e-10)
  }

  test("logDiff") {
    logDiff(log(5), log(2)) should be(log(3) plusOrMinus 1e-10)
    logDiff(log(5), log(5)) should be(Double.NegativeInfinity)

    evaluating {
      logDiff(log(5), log(6))
    } should produce[IllegalArgumentException]
  }

  import Arbitrary._;

  implicit def ae(x: Double) = new {
    def =~=(y: Double) = (x - y).abs / x < 1E-6;
  }

  // TODO 2.9 filter out Double.MaxValue.
  /*test("logsumming is approximately associative") {
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
      logSum(a,logSum(b,c)) =~= logSum(logSum(a,b),c);
    })
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
      logSum(a,logSum(b,c)) =~= logSum(a,b,c);
    })
  }

  test("sum distributes over logsum") {
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
      (a + logSum(b,c)) =~= (logSum(a + b,a+c));
    })
  }*/

}
