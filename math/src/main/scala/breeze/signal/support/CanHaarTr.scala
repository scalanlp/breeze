package breeze.signal.support

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Construction delegate for getting the FHT of a value of type InputType.
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call this implicit delegate directly.
 *
 * @author ktakagaki
 */
trait CanHaarTr[InputType, OutputType] {
  def apply(v1: InputType): OutputType
}

/**
 * Construction delegate for getting the FHT of a value of type InputType.
 */
object CanHaarTr {

  // TODO: is the fht any good if you use complex numbers?
  // the main computation (x + y) / 2, (x - y) / 2 would work with complex numbers
  // but I can neither find a reference saying it would be a good idea nor a lib
  // that uses it...

  // TODO: Int/Long versions are easy as long as one does not need to normalize.
  // The normalized version (deviding by 2 on each iteration) is recommended.
  // However an unnormalized version could run on Int/Long vectors.

  private val nFactor = 1d / Math.sqrt(2d)

  /**Copy or pad a given vector.
   */
  private def padOrCopy(v: DenseVector[Double]) = {
    if ((v.length & -v.length) == v.length) {
      v.copy
    } else {
      val length = 1 << (32 - Integer.numberOfLeadingZeros(v.length))
      val r = new DenseVector(new Array[Double](length))
      r.slice(0, v.length) := v
      r
    }
  }

  /** Transform a matrix into a squared power of 2 matrix.
   */
  private def squareMatrix(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    val maxd = Math.max(m.rows, m.cols)
    val rows = if ((maxd & -maxd) == maxd) {
      maxd
    } else {
      1 << (32 - Integer.numberOfLeadingZeros(Math.max(m.rows, m.cols)))
    }
    val o = DenseMatrix.zeros[Double](rows, rows)
    for (r <- 0 until m.rows; c <- 0 until m.cols) {
      o(r, c) = m(r, c)
    }
    o
  }

  /** Convert a dense matrix to a dense vector, concatenating all rows.
   */
  private def denseMatrixDToVector(m: DenseMatrix[Double]): DenseVector[Double] = {
    val v = new Array[Double](m.size)
    for (r <- 0 until m.rows; c <- 0 until m.cols) {
      val i = r * m.cols + c
      if (i < v.length) v(i) = m(r, c)
    }
    new DenseVector[Double](v)
  }

  /** Shape a DenseVector as a matric with a given number of rows/cols.
   */
  private def denseVectorDToMatrix(v: DenseVector[Double], rows: Int, cols: Int): DenseMatrix[Double] = {
    val m = DenseMatrix.zeros[Double](rows, cols)
    for (r <- 0 until m.rows; c <- 0 until m.cols) {
      m(r, c) = v(r * cols + c)
    }
    m
  }

  /** Compute the fht on a given double vector.
   */
  implicit val dvDouble1FHT: CanHaarTr[DenseVector[Double], DenseVector[Double]] = {
    new CanHaarTr[DenseVector[Double], DenseVector[Double]] {
      def apply(v: DenseVector[Double]) = {
        def _fht(v: DenseVector[Double]): DenseVector[Double] = {
          if (v.length > 1) {
            val p = v.toArray.grouped(2).toList
            v.slice(0, v.length / 2) := _fht(new DenseVector(p.map(e => (e(0) + e(1)) * nFactor).toArray))
            v.slice(v.length / 2, v.length) := new DenseVector(p.map(e => (e(0) - e(1)) * nFactor).toArray)
          }
          v
        }
        _fht(padOrCopy(v))
      }
    }
  }

  /** Compute the fht on a given double matrix.
   */
  implicit val dmDouble1FHT: CanHaarTr[DenseMatrix[Double], DenseMatrix[Double]] = {
    new CanHaarTr[DenseMatrix[Double], DenseMatrix[Double]] {
      def apply(m: DenseMatrix[Double]) = {
        def _fht(m: DenseMatrix[Double], limit: Int): Unit = if (limit > 1) {
          for (c <- 0 until limit) {
            val p = m(::, c).slice(0, limit).toArray.grouped(2).toArray
            val v = p.map(e => (e(0) + e(1)) * nFactor) ++ p.map(e => (e(0) - e(1)) * nFactor)
            for (r <- 0 until limit) m(r, c) = v(r)
          }
          for (r <- 0 until limit) {
            // m(r, ::).t(::, 0) is the same as m.t(::, r)
            // dv.slice(0,limit) is the same as dv(0 until limit)
            val p = m.t(0 until limit, r).toArray.grouped(2).toArray
            val v = p.map(e => (e(0) + e(1)) * nFactor) ++ p.map(e => (e(0) - e(1)) * nFactor)
            for (c <- 0 until limit) m(r, c) = v(c)
          }
          _fht(m, limit / 2)
        }

        val v = squareMatrix(m)
        _fht(v, v.rows)
        v
      }
    }
  }

}
