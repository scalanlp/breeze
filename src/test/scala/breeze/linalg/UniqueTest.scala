package breeze.linalg

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import spire.implicits._

/**
 * @author stucchio
 */
class uniqueTest extends FunSuite {
  test("unique works") {
    val v = DenseVector(1.0, 1.0, 2.0, 3.0, 3.0, 1.0, 5.0)
    assert(unique(v) == DenseVector(1.0, 2.0, 3.0, 5.0))
  }

  test("unique works with 1 element") {
    val v = DenseVector(1.0)
    assert(unique(v) == DenseVector(1.0))
  }

  test("unique works with 0 elements") {
    val v = DenseVector[Double]()
    assert(unique(v) == DenseVector[Double]())
  }

}
