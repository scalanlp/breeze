package breeze.linalg

import org.scalatest._
import org.scalatest.funsuite._

class fliplrTest extends AnyFunSuite {
  test("fliplr is its own inverse") {
    val m = DenseMatrix.rand(10, 8)
    assert(fliplr(m) != m)
    assert(fliplr(fliplr(m)) == m)
  }

}
