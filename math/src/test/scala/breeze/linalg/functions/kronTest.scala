package breeze.linalg
package functions

import breeze.math.Complex
import org.scalatest.FunSuite

class kronTest extends FunSuite {

  test("kron complex") {
    val m = DenseMatrix((Complex(0, 1), Complex(2,1)), (Complex(3, 3), Complex(4, 4)))
    val result = DenseMatrix(
      (Complex(-1.0, 0.0), Complex(-1.0, 2.0), Complex(-1.0, 2.0), Complex(3.0, 4.0)),
      (Complex(-3.0, 3.0), Complex(-4.0, 4.0), Complex(3.0, 9.0), Complex(4.0, 12.0)),
      (Complex(-3.0, 3.0), Complex(3.0, 9.0), Complex(-4.0, 4.0), Complex(4.0, 12.0)),
      (Complex(0.0, 18.0), Complex(0.0, 24.0), Complex(0.0, 24.0), Complex(0.0, 32.0))
    )
    assert(kron(m, m) == result)
  }

}
