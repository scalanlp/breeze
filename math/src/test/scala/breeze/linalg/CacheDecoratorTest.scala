package breeze.linalg

import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith
import org.junit.Assert.assertEquals
import breeze.math.{DoubleValuedTensorSpaceTestBase, TensorSpace, TensorSpaceTestBase}

import scala.math.sqrt


/**
 *
 * @author fozziethebeat
 */
@RunWith(classOf[JUnitRunner])
class CacheDecoratorTest extends FunSuite with Checkers {

  val TOLERANCE = 1e-4

  def assertClose(a: Double, b: Double) =
    assert(math.abs(a - b) < TOLERANCE)

  test("Can find dense binaryOps") {
    val v = DenseVector(2, 0, 3, 2, -1)
    val vd = new CacheDecorator(v)
    assert( (v + 5) == (vd + 5))
    assert( (v * 5) == (vd * 5))
    assert( (v - 5) == (vd - 5))
    assert( (v / 5) == (vd / 5))
    assert( (v :^ 5) == (vd :^ 5))
  }

  test("Can find sparse binaryOps") {
    val v = SparseVector(2, 0, 3, 2, -1)
    val vd = new CacheDecorator(v)
    assert( (v + 5) == (vd + 5))
    assert( (v * 5) == (vd * 5))
    assert( (v - 5) == (vd - 5))
  }

  test("Can find sparse active iterator") {
    val v = SparseVector(10)( (0, 2), (3, 0), (4, 3), (6, 2), (8, -1) )
    val vd = new CacheDecorator(v)
    val size = vd.activeIterator.size
    assertEquals(5, size)
  }

  test("Can find dense active iterator") {
    val v = DenseVector(2, 0, 3, 2, -1)
    val vd = new CacheDecorator(v)
    val size = vd.activeIterator.size
    assertEquals(5, size)
  }

  test("Update Cached DenseVector") {
    val v = DenseVector(2, 0, 3, 2, -1)
    val vd = new CacheDecorator(v)
    vd(1) = 4
    assertClose(v(1), vd(1))
    assertClose(4, vd(1))
  }

  test("Update Cached SparseVector") {
    val v = SparseVector(10)( (0, 2), (3, 0), (4, 3), (6, 2), (8, -1) )
    val vd = new CacheDecorator(v)
    vd(1) = 4
    assertClose(v(1), vd(1))
    assertClose(4, vd(1))
  }

  test("Cached sum") {
    val v = SparseVector(10)( (0, 2), (3, 0), (4, 3), (6, 2), (8, -1) )
    val vd = new CacheDecorator(v)
    assertEquals(6, vd.sum, TOLERANCE)
    assertEquals(6, vd.sum, TOLERANCE)
  }

  test("Cached Norms") {
    val v = SparseVector(10)( (0, 2), (3, 0), (4, 3), (6, 2), (8, -1) )
    val vd = new CacheDecorator(v)
    assertEquals(8, vd.norm(1), TOLERANCE)
    assertEquals(8, vd.norm(1), TOLERANCE)
    assertEquals(sqrt(18), vd.norm(2), TOLERANCE)
    assertEquals(sqrt(18), vd.norm(2), TOLERANCE)
  }

  test("Update invalidates cache") {
    val v = DenseVector(2, 0, 3, 2, -1)
    val vd = new CacheDecorator(v)
    assertClose(8, vd.norm(1))
    assertClose(6, vd.sum)
    vd(1) = 1
    assertClose(9, vd.norm(1))
    assertClose(7, vd.sum)
  }
}
