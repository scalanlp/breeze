package breeze.features

import breeze.linalg._
import org.scalatest.funsuite.AnyFunSuite
import breeze.linalg.DenseVector

class FeatureVectorTest extends AnyFunSuite {
  test("axpy fv dv") {
    val f = FeatureVector(3, 4, 5, 6, 10)
    val dv = DenseVector.zeros[Double](12)
    axpy(3.0, f, dv)
    assert(dv === DenseVector.tabulate(12)(p => if (f.data.contains(p)) 3.0 else 0.0))
  }

  test("axpy fv vb") {
    val f = FeatureVector(3, 4, 5, 6, 10)
    val dv = VectorBuilder.zeros[Double](12)
    axpy(3.0, f, dv)
    assert(dv.toDenseVector === DenseVector.tabulate(12)(p => if (f.data.contains(p)) 3.0 else 0.0))
  }

  test("DM mult") {
    val f = FeatureVector(3, 4, 5, 6, 10)
    val dm = DenseMatrix.rand(12, 13)
    val res = dm * f
    assert(res === dm * SparseVector(13)(f.data.map(_ -> 1.0): _*))
  }

  test("CSC mult") {
    val f = FeatureVector(3, 4, 5, 6, 10)
    val dm = CSCMatrix.rand(12, 13)
    val res = dm * f
    assert(res === dm * SparseVector(13)(f.data.map(_ -> 1.0): _*))
  }

  test("DM trans mult") {
    val f = FeatureVector(3, 4, 5, 6, 10)
    val dm = DenseMatrix.rand(12, 13)
    val res = f.t * dm.t

    assert(res.t === dm * SparseVector(13)(f.data.map(_ -> 1.0): _*))

  }

}
