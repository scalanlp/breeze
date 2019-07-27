package breeze.linalg

import breeze.linalg.operators.OpMulInner
import org.scalatest.FunSuite
import breeze.stats.mean

/**
 * TODO
 *
 * @author dlwh
 **/
class BroadcastedTest extends FunSuite {
  test("broadcast DenseMatrix along columns") {
    val dm = DenseMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))

    val res = dm(::, *) + DenseVector(3.0, 4.0)
    assert(res === DenseMatrix((4.0, 5.0, 6.0), (8.0, 9.0, 10.0)))

    val comp = dm(::, *) <:< DenseVector(3.0, 4.0)
    assert(comp === DenseMatrix((true, true, false), (false, false, false)))

    res(::, *) := DenseVector(3.0, 4.0)
    assert(res === DenseMatrix((3.0, 3.0, 3.0), (4.0, 4.0, 4.0)))
  }

  test("broadcast slice DenseMatrix along columns") {
    val dm = DenseMatrix((-1.0, -2.0, -3.0), (1.0, 2.0, 3.0), (4.0, 5.0, 6.0))

    dm(1 to 2, *) := DenseVector(3.0, 4.0)
    assert(dm === DenseMatrix((-1.0, -2.0, -3.0), (3.0, 3.0, 3.0), (4.0, 4.0, 4.0)))
  }

  test("mean") {
    val m = DenseMatrix((1.0, 3.0), (4.0, 4.0))
    assert(mean(m(*, ::)) === DenseVector(2.0, 4.0))
    assert(mean(m(::, *)) === DenseVector(2.5, 3.5).t)
  }

  test("broadcast map to normalize") {
    val dm = DenseMatrix((-1.0, -2.0, -3.0), (1.0, 2.0, 3.0), (4.0, 8.0, 7.0))

    val res = dm(1 to 2, *).map(v => v / sum(v))

    assert(res === DenseMatrix((0.2, 0.2, 0.3), (0.8, 0.8, 0.7)))
  }

  test("broadcast map to normalize rows") {
    val dm = DenseMatrix((-2.0, -2.0, -3.0), (1.0, 4.0, 3.0), (4.0, 6.0, 7.0))

    val res = dm(*, 0 to 1).map(v => v / sum(v))

    assert(res === DenseMatrix((0.5, 0.5), (0.2, 0.8), (0.4, 0.6)))
  }

  test("broadcast DenseMatrix along rows") {
    val dm = DenseMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0)).t

    val res = dm(*, ::) + DenseVector(3.0, 4.0)
    assert(res === DenseMatrix((4.0, 5.0, 6.0), (8.0, 9.0, 10.0)).t)

    val comp = dm(*, ::) <:< DenseVector(3.0, 4.0)
    assert(comp === DenseMatrix((true, true, false), (false, false, false)).t)

    res(*, ::) := DenseVector(3.0, 4.0)
    assert(res === DenseMatrix((3.0, 3.0, 3.0), (4.0, 4.0, 4.0)).t)
  }

  test("broadcast slice DenseMatrix along rows") {
    val dm = DenseMatrix((-1.0, -2.0, -3.0), (1.0, 2.0, 3.0), (4.0, 5.0, 6.0)).t

    dm(*, 1 to 2) := DenseVector(3.0, 4.0)
    assert(dm === DenseMatrix((-1.0, -2.0, -3.0), (3.0, 3.0, 3.0), (4.0, 4.0, 4.0)).t)
  }

  test("dot product for DM/FV") {
    val dm = DenseMatrix((-1.0, -2.0, -3.0), (1.0, 2.0, 3.0), (4.0, 5.0, 6.0))

    import breeze.features._
//    implicit val bc = BroadcastedRows.broadcastOp2[OpMulInner.type, DenseMatrix[Double], DenseVector[Double], FeatureVector, Double, DenseVector[Double]]
    val r = new FeatureVector(Array(1, 2)).dot(dm(*, ::))
    assert(r === DenseVector(-5.0, 5.0, 11.0))
  }

  test("Counter sum") {
    val ctr = Counter2(("a", 1, 2.0), ("b", 1, 3.0), ("b", 4, 5.0))
    assert(sum(ctr(*, ::)) === Counter("b" -> 8.0, "a" -> 2.0))
    assert(sum(ctr(::, *)) === Counter(1 -> 5.0, 4 -> 5.0))
  }

  test("foreach") {
    val dm = DenseMatrix((-1.0, -2.0, -3.0), (1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    var sum = 0.0
    dm(*, ::).foreach(sum += _(1))
    assert(sum == 5)

    sum = 0.0
    dm(::, *).foreach(sum += _(1))
    assert(sum == 6)

  }

  test("fold") {
    val dm = DenseMatrix((-1.0, -2.0, -3.0), (1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    assert(dm(*, ::).foldLeft(0.0)(_ + _(1)) == 5)
    assert(dm(::, *).foldLeft(0.0)(_ + _(1)) == 6)

  }

  test("broadcasted toIndexedSeq") {
    val dm = DenseMatrix((-1.0, -2.0, -3.0), (1.0, 2.0, 3.0), (4.0, 5.0, 6.0))

    assert(dm(::, *).toIndexedSeq == dm(::, *).iterator.toIndexedSeq)
    assert(dm(*, ::).toIndexedSeq.map(_.t) == dm(*, ::).iterator.toIndexedSeq)
  }

  test("broadcasted min/max") {
    val b = DenseMatrix.rand(3, 3);
    val b2 = DenseVector(3.0, 4.0, 5.0)

    val c = max(b(*, ::), b2)
    assert(c == DenseMatrix.vertcat(b2.toDenseMatrix, b2.toDenseMatrix, b2.toDenseMatrix))
    val c2 = max(b(::, *), b2)
    assert(c2 == DenseVector.horzcat(b2, b2, b2))

    val d = min(b(*, ::), -b2)
    assert(d == DenseMatrix.vertcat(-b2.toDenseMatrix, -b2.toDenseMatrix, -b2.toDenseMatrix))
    val d2 = min(b(::, *), -b2)
    assert(d2 == DenseVector.horzcat(-b2, -b2, -b2))
  }

  test("broadcasted RHS") {
    val v = DenseVector.rand(3)
    val m = DenseMatrix.rand(3, 4)
    v /:/ m(::, *)
  }

}
