package breeze.optimize

import org.scalatest.FunSuite

class DiffFunctionTest extends FunSuite {

  object f1 extends DiffFunction[Double] {
    override def calculate(x: Double): (Double, Double) = {
      (x, 1.0)
    }
  }

  object f2 extends DiffFunction[Double] {
    override def calculate(x: Double): (Double, Double) = {
      (x * x / 2.0, x)
    }
  }

  test("simple ops: add") {
    val f12 = f1 + f2
    assert(f12(1) == f1(1) + f2(1))
    assert(f12.gradientAt(1) == f1.gradientAt(1) + f2.gradientAt(1))
  }

  test("simple ops: sub") {

    val f1M2 = f1 - f2
    assert(f1M2(1) == f1(1) - f2(1))
    assert(f1M2.gradientAt(1) == f1.gradientAt(1) - f2.gradientAt(1))
  }

  test("simple ops: mul") {
    val f1S = DiffFunction.opMulDiffFunction[Double].apply(f1, 3.0)
    assert(f1S(1) == f1(1) * 3.0)
    assert(f1S.gradientAt(1) == f1.gradientAt(1) * 3.0)
  }

}
