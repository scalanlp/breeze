package breeze.linalg

import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith
import org.junit.Assert.assertEquals
import breeze.math.{DoubleValuedTensorSpaceTestBase, TensorSpace, TensorSpaceTestBase}

/**
 *
 * @author fozziethebeat
 */
@RunWith(classOf[JUnitRunner])
class VectorDecoratorTest extends FunSuite with Checkers {

  test("Can find dense binaryOps") {
    val v = DenseVector(2, 0, 3, 2, -1)
    val vd = new VectorDecorator(v)
    assert( (v + 5) == (vd + 5))
    assert( (v * 5) == (vd * 5))
    assert( (v - 5) == (vd - 5))
    assert( (v / 5) == (vd / 5))
    assert( (v :^ 5) == (vd :^ 5))
  }

  test("Can find sparse binaryOps") {
    val v = SparseVector(2, 0, 3, 2, -1)
    val vd = new VectorDecorator(v)
    assert( (v + 5) == (vd + 5))
    assert( (v * 5) == (vd * 5))
    assert( (v - 5) == (vd - 5))
  }

  test("Can find sparse active iterator") {
    val v = SparseVector(10)( (0, 2), (3, 0), (4, 3), (6, 2), (8, -1) )
    val vd = new VectorDecorator(v)
    val size = vd.activeIterator.size
    assertEquals(5, size)
  }

  test("Can find dense active iterator") {
    val v = DenseVector(2, 0, 3, 2, -1)
    val vd = new VectorDecorator(v)
    val size = vd.activeIterator.size
    assertEquals(5, size)
  }
}
