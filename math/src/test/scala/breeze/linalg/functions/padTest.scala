package breeze.linalg

import org.scalatest.FunSuite

/**
 * @author ktakagaki
 * @date 04/16/2014.
 */
class padTest extends FunSuite {

  val testDVI = DenseVector(1, 2, 3, 4, 5)
  val testDMI = DenseMatrix(1, 2, 3, 4, 5)
  val testDVD = DenseVector(1d, 2d, 3d, 4d, 5d)

  test("padRight") {

    import breeze.linalg.Options._

    assert(padRight(testDVI, dimensions = 3) == DenseVector(1, 2, 3))
    assert(padRight(testDVD, dimensions = 7, mode = 11d) == DenseVector(1d, 2d, 3d, 4d, 5d, 11d, 11d))
    assert(padRight(testDVD, 7, 10d) == DenseVector(1d, 2d, 3d, 4d, 5d, 10d, 10d))
    assert(padRight(testDVD, 7, Value(10d)) == DenseVector(1d, 2d, 3d, 4d, 5d, 10d, 10d))
    assert(padRight(testDVI, 7, Min) == DenseVector(1, 2, 3, 4, 5, 1, 1))
    assert(padRight(testDVI, 7, Max) == DenseVector(1, 2, 3, 4, 5, 5, 5))
    assert(padRight(testDVI + 1, 7, Mean) == DenseVector(2, 3, 4, 5, 6, 4, 4))
    assert(padRight(testDVI, 7, Median) == DenseVector(1, 2, 3, 4, 5, 3, 3))
    assert(padRight(testDVI, 7, Wrap) == DenseVector(1, 2, 3, 4, 5, 1, 2))
    assert(padRight(testDVI, 7, Reflect) == DenseVector(1, 2, 3, 4, 5, 5, 4))

    assert(
      padRight(testDMI, dimensions = (7, 3), 0) == DenseMatrix(
        (1, 0, 0),
        (2, 0, 0),
        (3, 0, 0),
        (4, 0, 0),
        (5, 0, 0),
        (0, 0, 0),
        (0, 0, 0)
      )
    )
    assert(padRight(testDMI, dimensions = (3, 3), 0) == DenseMatrix((1, 0, 0), (2, 0, 0), (3, 0, 0)))

    assert(padLeft(testDVI, dimensions = 3) == DenseVector(3, 4, 5))
    assert(padLeft(testDVD, dimensions = 7, mode = 11d) == DenseVector(11d, 11d, 1d, 2d, 3d, 4d, 5d))
    assert(padLeft(testDVD, 7, 10d) == DenseVector(10d, 10d, 1d, 2d, 3d, 4d, 5d))
    assert(padLeft(testDVD, 7, Value(10d)) == DenseVector(10d, 10d, 1d, 2d, 3d, 4d, 5d))
    assert(padLeft(testDVI, 7, Min) == DenseVector(1, 1, 1, 2, 3, 4, 5))
    assert(padLeft(testDVI, 7, Max) == DenseVector(5, 5, 1, 2, 3, 4, 5))
    assert(padLeft(testDVI + 1, 7, Mean) == DenseVector(4, 4, 2, 3, 4, 5, 6))
    assert(padLeft(testDVI, 7, Median) == DenseVector(3, 3, 1, 2, 3, 4, 5))
    assert(padLeft(testDVI, 7, Wrap) == DenseVector(4, 5, 1, 2, 3, 4, 5))
    assert(padLeft(testDVI, 7, Reflect) == DenseVector(2, 1, 1, 2, 3, 4, 5))

    assert(
      padLeft(testDMI, dimensions = (7, 3), 0) == DenseMatrix(
        (0, 0, 0),
        (0, 0, 0),
        (0, 0, 1),
        (0, 0, 2),
        (0, 0, 3),
        (0, 0, 4),
        (0, 0, 5)
      )
    )
    assert(padLeft(testDMI, dimensions = (3, 3), 0) == DenseMatrix((0, 0, 3), (0, 0, 4), (0, 0, 5)))

  }

}
