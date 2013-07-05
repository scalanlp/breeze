package breeze.linalg

import org.scalatest.FunSuite
import breeze.generic.CanCollapseAxis
import breeze.linalg.operators.{OpAdd, BinaryOp}

/**
 * TODO
 *
 * @author dlwh
 **/
class BroadcastedTest extends FunSuite {
  test("broadcast DenseMatrix along columns") {
    val dm = DenseMatrix((1.0,2.0,3.0),
                         (4.0,5.0,6.0))

    val res = dm(::, *) + DenseVector(3.0, 4.0)
    assert(res === DenseMatrix((4.0, 5.0, 6.0), (8.0, 9.0, 10.0)))

    res(::, *) := DenseVector(3.0, 4.0)
    assert(res === DenseMatrix((3.0, 3.0, 3.0), (4.0, 4.0, 4.0)))
  }

  test("broadcast slice DenseMatrix along columns") {
    val dm = DenseMatrix((-1.0,-2.0,-3.0),
      (1.0,2.0,3.0),
      (4.0,5.0,6.0))

    dm(1 to 2, *) := DenseVector(3.0, 4.0)
    assert(dm === DenseMatrix((-1.0,-2.0,-3.0), (3.0, 3.0, 3.0), (4.0, 4.0, 4.0)))
  }

}
