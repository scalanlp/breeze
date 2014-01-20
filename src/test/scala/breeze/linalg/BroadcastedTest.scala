package breeze.linalg

import org.scalatest.FunSuite

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


    val comp = dm(::, *) :< DenseVector(3.0, 4.0)
    assert(comp === DenseMatrix((true, true, false), (false, false, false)) )

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

  test("mean") {
    val m = DenseMatrix((1.0, 3.0), (4.0, 4.0))
    assert(mean(m(*, ::)) === DenseVector(2.0, 4.0))
    assert(mean(m(::, *)) === DenseMatrix((2.5, 3.5)))
  }

  test("broadcast map to normalize") {
    val dm = DenseMatrix((-1.0,-2.0,-3.0),
      (1.0,2.0,3.0),
      (4.0,8.0,7.0))

    val res = dm(1 to 2, *).map( v => v/ sum(v))

    assert(res === DenseMatrix((0.2, 0.2, 0.3), (0.8, 0.8, 0.7)))
  }

  test("broadcast map to normalize rows") {
    val dm = DenseMatrix((-2.0,-2.0,-3.0),
      (1.0,4.0,3.0),
      (4.0,6.0,7.0))

    val res = dm(*, 0 to 1).map( v => v/ sum(v))

    assert(res === DenseMatrix((0.5, 0.5), (0.2, 0.8), (0.4, 0.6)))
  }

  test("broadcast DenseMatrix along rows") {
    val dm = DenseMatrix((1.0,2.0,3.0),
      (4.0,5.0,6.0)).t

    val res = dm(*, ::) + DenseVector(3.0, 4.0)
    assert(res === DenseMatrix((4.0, 5.0, 6.0), (8.0, 9.0, 10.0)).t)

    val comp = dm(*, ::) :< DenseVector(3.0, 4.0)
    assert(comp === DenseMatrix((true, true, false), (false, false, false)).t )


    res(*, ::) := DenseVector(3.0, 4.0)
    assert(res === DenseMatrix((3.0, 3.0, 3.0), (4.0, 4.0, 4.0)).t)
  }

  test("broadcast slice DenseMatrix along rows") {
    val dm = DenseMatrix((-1.0,-2.0,-3.0),
      (1.0,2.0,3.0),
      (4.0,5.0,6.0)).t

    dm(*, 1 to 2) := DenseVector(3.0, 4.0)
    assert(dm === DenseMatrix((-1.0,-2.0,-3.0), (3.0, 3.0, 3.0), (4.0, 4.0, 4.0)).t)
  }

  test("dot product for DM/FV") {
    val dm = DenseMatrix((-1.0,-2.0,-3.0),
      (1.0,2.0,3.0),
      (4.0,5.0,6.0))

    import breeze.features._
    val r = dm(*, ::) dot new FeatureVector(Array(1,2))
    assert(r === DenseVector(-5.0, 5.0, 11.0))
  }

  test("Counter sum") {
    val ctr = Counter2( ('a, 1, 2.0), ('b, 1, 3.0), ('b, 4, 5.0))
    assert(sum(ctr(*, ::)) ===   Counter('b -> 8.0, 'a -> 2.0))
    assert(sum(ctr(::, *)) ===   Counter(1-> 5.0, 4 -> 5.0))
  }

}
