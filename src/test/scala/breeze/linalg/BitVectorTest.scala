package breeze.linalg

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class BitVectorTest extends FunSuite {
  test("Or") {
    val as = BitVector(10)(1, 3, 5, 7)
    val bs = BitVector(10)(1, 2, 3, 5)
    assert( (as | bs) === BitVector(10)(1, 2, 3, 5, 7))
  }


  test("And") {
    val as = BitVector(10)(1, 3, 5, 7)
    val bs = BitVector(10)(1, 2, 3, 5)
    assert( (as & bs) === BitVector(10)(1, 3, 5))
  }

  test("Xor") {
    val as = BitVector(10)(1, 3, 5, 7)
    val bs = BitVector(10)(1, 2, 3, 5)
    assert( (as ^^ bs) === BitVector(10)(2, 7))
  }

  test("Eq") {
    val as = BitVector(10)(1, 3, 5, 7)
    val bs = BitVector(10)(1, 2, 3, 5)
    assert( (as :== bs) === BitVector(10)(0, 1, 3, 4, 5, 6, 8,9))
  }

  test("Ne") {
    val as = BitVector(10)(1, 3, 5, 7)
    val bs = BitVector(10)(1, 2, 3, 5)
    assert( (as :!= bs) === BitVector(10)(2, 7))
  }

  test("Op Not") {
    val a = BitVector(10)(2, 7)
    assert(!a === BitVector(10)(0,1,3,4, 5,6, 8,9))
  }

}
