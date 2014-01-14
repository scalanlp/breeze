package breeze.linalg

import org.scalatest.FunSuite
import breeze.linalg.support.CanTraverseValues
import breeze.math.Semiring
import breeze.linalg.support.CanTraverseValues.ValuesVisitor

/**
 * TODO
 *
 * @author dlwh
 **/
class BitVectorTest extends FunSuite {
  test("Ones") {
    val as = BitVector.ones(5)
    val expected = BitVector(5)(0, 1, 2, 3, 4)
    assert( as === expected)
  }

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

  test("MulInner") {
    val a = BitVector   (false, false, true  , true,   false, true, true)
    val b = SparseVector(1,     0,      2,        0,       3, 4, 0)
    val bd = DenseVector(1,     0,      2,        0,       3, 4, 0)
    b.compact()
    assert( (a dot b) === (b dot a))
    assert( (a dot b) === 6)
    assert( (a dot bd) === 6)
  }

  test("axpy") {
    val a = BitVector   (false, false, true  , true,   false, true, true)
    val b = SparseVector(1,     0,      2,        0,       3, 4, 0)
    b.compact()
    val bd = DenseVector(1,     0,      2,        0,       3, 4, 0)
    axpy(3, a, b)
    axpy(3, a, bd)
    assert(b === SparseVector(1, 0, 5, 3, 3, 7, 3))
    assert(bd === DenseVector(1, 0, 5, 3, 3, 7, 3))
  }

  test("sum") {
    assert(sum(BitVector(false, false, true)) === true)
    assert(sum(BitVector(false, false, false)) === false)
  }

}
