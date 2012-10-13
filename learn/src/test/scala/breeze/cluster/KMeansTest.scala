package breeze.cluster

import org.scalatest.FunSuite
import breeze.linalg.DenseVector
import breeze.numerics._

/**
 *
 * @author dlwh
 */
class KMeansTest extends FunSuite {
  test("Scalabha kmeans test") {
    val clusters =
      IndexedSeq(
        IndexedSeq((DenseVector(0.0, 0.0)), (DenseVector(0.0, 1.0)), (DenseVector(1.0, 0.0)), (DenseVector(1.0, 1.0))),
        IndexedSeq((DenseVector(8.0, 8.0)), (DenseVector(8.0, 9.0)), (DenseVector(9.0, 8.0)), (DenseVector(9.0, 9.0))),
        IndexedSeq((DenseVector(0.0, 8.0)), (DenseVector(0.0, 9.0)), (DenseVector(1.0, 8.0)), (DenseVector(1.0, 9.0))))
    val points = clusters.flatten

    val kmeans = new KMeans[DenseVector[Double]](3, 1E-4)

    val state = Array.fill(10)(kmeans.cluster(points)).minBy(_.error)

    assert(closeTo(state.error,6.0),state.error)
    val IndexedSeq(a, b, c) = state.means.sortWith((a,b) => if(a(0) < b(0)) true else if(a(0) > b(0)) false else a(1) < b(1))
    assert((DenseVector(0.5, 0.5)) === a)
    assert((DenseVector(0.5, 8.5)) === b)
    assert((DenseVector(8.5, 8.5)) === c)
  }


}
