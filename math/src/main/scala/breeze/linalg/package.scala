package breeze

/**
 *
 * @author dlwh
 */
package object linalg {

  /**
   * returns a vector along the diagonal of v.
   * Requires a square matrix?
   * @param m the matrix
   * @tparam V
   */
  def diag[V](m: DenseMatrix[V]) = {
    require(m.rows == m.cols, "m must be square")
    new DenseVector(m.data, m.offset, m.majorStride + 1, m.rows)
  }

}

