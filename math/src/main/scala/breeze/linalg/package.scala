package breeze

import linalg.operators.{BinaryOp, OpDiv}
import linalg.support.{CanNorm, CanCopy}

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

  /**
   * Generates a vector of linearly spaced values between a and b (inclusive).
   * The returned vector will have length elements, defaulting to 100.
   */
  def linspace(a : Double, b : Double, length : Int = 100) : DenseVector[Double] = {
    val increment = (b - a) / (length - 1)
    DenseVector.tabulate(length)(i => a + increment * i)
  }

  def copy[T](t: T)(implicit canCopy: CanCopy[T]): T = canCopy(t)

  def norm[T](t: T, v: Double = 2)(implicit canNorm: CanNorm[T]) = canNorm(t, v)

  def normalize[T, U](t: T, v: Double = 2)(implicit div: BinaryOp[T, Double, OpDiv, U], canNorm: CanNorm[T]): U = {
    div(t,canNorm(t, v))
  }

}

