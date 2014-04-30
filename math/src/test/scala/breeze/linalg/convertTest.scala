package breeze.linalg

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class convertTest extends FunSuite {
  test("Int -> {Double, Short}") {
    assert(convert(Array(1, 2, 3), Double).toIndexedSeq == IndexedSeq(1.0, 2.0, 3.0))
    assert(convert(Array(1, 2, 3), Short).toIndexedSeq == IndexedSeq[Short](1, 2, 3))
  }

  test("Double -> {Double, Short}") {
    assert(convert(Array(1.0, 2.0, 3.0), Double).toIndexedSeq == IndexedSeq(1.0, 2.0, 3.0))
    assert(convert(Array(1.0, 2.0, 3.0), Short).toIndexedSeq == IndexedSeq[Short](1, 2, 3))
  }

}
