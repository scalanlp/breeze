package breeze.linalg

import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

import breeze.collection.mutable.SparseArray

import scala.math.sqrt


/**
 * Test that the NormCacheVector can be mixed in with the two common vector types and that modifying the vector invalidates the cache.  Note
 * that it's difficult to test that this is in fact faster, we only test for numerical accuracy.  independent performance tests will be
 * needed to test time savings.
 *
 * @author fozziethebeat
 */
@RunWith(classOf[JUnitRunner])
class NormCacheTest extends FunSuite with Checkers {

  test("Can Mixin NormCacheVector with DenseVector") {
    val a = Array(2, 0, 3, 2, -1)
    val v = new DenseVector(a) with NormCacheVector[Int]
    assert(v.norm(2) == sqrt(18))
    assert(v.norm(2) == sqrt(18))
  }

  test("Can Mixin NormCacheVector with SparseVector") {
    val sa = SparseArray.create(5)( (0,2), (2, 3), (3, 2), (4, -1) )
    val v = new SparseVector(sa) with NormCacheVector[Int]
    assert(v.norm(2) == sqrt(18))
    assert(v.norm(2) == sqrt(18))
  }

  test("Update refreshes norm value in DenseVector") {
    val a = Array(2, 0, 3, 2, -1)
    val v = new DenseVector(a) with NormCacheVector[Int]
    assert(v.norm(2) == sqrt(18))
    v.update(4, 4)
    v(4) = 4
    assert(v.norm(2) == sqrt(33))
  }

  /*
  test("Update refreshes norm value in SparseVector") {
    val sa = SparseArray.create(5)( (0,2), (2, 3), (3, 2), (4, -1) )
    val v = new SparseVector(sa) with NormCacheVector[Int]
    assert(v.norm(2) == sqrt(18))
    v(4) = 4
    assert(v.norm(2) == sqrt(33))
  }
  */
}
