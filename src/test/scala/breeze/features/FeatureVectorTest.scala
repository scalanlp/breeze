package breeze.features

import breeze.linalg._
import org.scalatest.FunSuite
import breeze.linalg.DenseVector

/**
 * TODO
 *
 * @author dlwh
 **/
class FeatureVectorTest extends FunSuite {
  test("axpy fv dv") {
    val f = FeatureVector(3,4,5,6,10)
    val dv = DenseVector.zeros[Double](12)
    axpy(3.0, f, dv)
    assert(dv === DenseVector.tabulate(12)(p => if(f.data.contains(p)) 3.0 else 0.0))
  }

  test("axpy fv vb") {
    val f = FeatureVector(3,4,5,6,10)
    val dv = VectorBuilder.zeros[Double](12)
    axpy(3.0, f, dv)
    assert(dv.toDenseVector === DenseVector.tabulate(12)(p => if(f.data.contains(p)) 3.0 else 0.0))
  }

}
