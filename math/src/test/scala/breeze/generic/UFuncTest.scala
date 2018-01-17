package breeze.generic

import breeze.linalg.{*, DenseMatrix, DenseVector}
import org.scalatest.FunSuite

/**
 * Created by dlwh on 10/21/15.
 */
class UFuncTest extends FunSuite {

  test("WrappedUFunc") {
    val f = UFunc(math.exp _)
    assert(f(3.0) == math.exp(3.0))
    assert(f(DenseVector(3.0)) == DenseVector(math.exp(3.0)))
    assert(f(DenseMatrix(3.0)) == DenseMatrix(math.exp(3.0)))
  }

  test("WrappedUFunc2") {
    val f = UFunc(math.pow _)
    assert(f(3.0, 4.0) == math.pow(3.0, 4.0))
    assert(f(DenseVector(3.0), 4.0) == DenseVector(math.pow(3.0, 4.0)))
    assert(f(DenseMatrix(3.0), DenseMatrix(4.0)) == DenseMatrix(math.pow(3.0, 4.0)))
  }

  test("UFunc with broadcasting") {
    val f = UFunc(math.pow _)
    val m = DenseMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))

    assert(f(m(::, *), m(::, 1)) == DenseMatrix((1.0, 4.0, 9.0), (1024.0, 3125.0, 7776.0)))
  }

}
