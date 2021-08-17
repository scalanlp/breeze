package breeze.linalg

import org.scalacheck.Prop
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

/**
 * Created by dlwh on 9/18/15.
 */
class argsortTest extends AnyFunSuite with Checkers {
  test("argsort dv") {
    check(Prop.forAll { (array: Array[Double]) =>
      val ax = argsort(new DenseVector(array))
      ax.toIndexedSeq.map(array) == array.sorted.toIndexedSeq
    })
  }

}

class argtopkTest extends AnyFunSuite {

  test("argtopk vector") {
    val dv = DenseVector(2, 0, 3, 2, -1)
    assert(argtopk(dv, 0) === Seq.empty)
    assert(argtopk(dv, 1) === Seq(2))
    assert(argtopk(dv, 3).toSet === Set(0, 2, 3))
    assert(argtopk(dv, 5).toSet === Set(0, 1, 2, 3, 4))

    val sv = SparseVector(5)(0 -> 2, 2 -> 3, 3 -> 2)
    assert(argtopk(sv, 0) === Seq.empty)
    assert(argtopk(sv, 1) === Seq(2))
    assert(argtopk(sv, 3).toSet === Set(0, 2, 3))
    assert(argtopk(sv, 5).toSet === Set(0, 1, 2, 3, 4))
  }

  test("#679") {
    var a = DenseMatrix((3, 1), (-1, -2), (2, 2), (5, 5))
    for (i <- 0 until 10)
      assert(argtopk(a(::, 0), 4).toIndexedSeq == argtopk(a(::, 0), 4).toIndexedSeq)
  }
}
