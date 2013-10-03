package breeze.linalg

import org.scalatest.FunSuite

/**
 *
 * @author dlwh
 */
class SliceVectorTest extends FunSuite {

  test("basic slices of a counter") {
    val ctr = Counter('a -> 1, 'b -> 2, 'c -> 42)
    val v:Vector[Int] = ctr(Seq('c, 'b))
    assert(v(0) === 42)
    assert(v(1) === 2)

    v(0) = 10
    assert(ctr('c) === 10)
    ctr('b) = 1
    assert(v(1) === 1)
  }

}
