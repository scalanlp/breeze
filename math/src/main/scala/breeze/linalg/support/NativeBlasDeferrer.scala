package breeze.linalg.support

import org.jblas.NativeBlas

/**
 * Layer of indirection to help with link errors when libraries aren't around.
 * @author dlwh
 */
object NativeBlasDeferrer {
  def dgesv(n: Int, nrhs: Int, a: Array[Double], aIdx: Int, lda: Int, ipiv: Array[Int], ipivIdx: Int, b: Array[Double], bIdx: Int, ldb: Int): Int = {
    NativeBlas.dgesv(n, nrhs, a, aIdx, lda, ipiv, ipivIdx, b, bIdx, ldb)
  }

  def dgetrf(i: Int, i1: Int, doubles: Array[Double], i2: Int, i3: Int, ints: Array[Int], i4: Int) = {
    NativeBlas.dgetrf(i, i1, doubles, i2, i3, ints, i4)
  }

  def dgeev(c: Char, c1: Char, i: Int, doubles: Array[Double],
            i1: Int, i2: Int, doubles1: Array[Double], i3: Int,
            doubles2: Array[Double], i4: Int, doubles3: Array[Double],
            i5: Int, i6: Int, doubles4: Array[Double], i7: Int, i8: Int) = {
    assert(breeze.linalg.canLoadNativeBlas)
    NativeBlas.dgeev(c, c1, i, doubles, i1, i2, doubles1,
      i3, doubles2, i4, doubles3, i5, i6, doubles4, i7, i8)
  }

  def dsyev(c: Char, c1: Char, i: Int, doubles: Array[Double], i1: Int, i2: Int, doubles1: Array[Double], i3: Int) = {
    assert(breeze.linalg.canLoadNativeBlas)
    NativeBlas.dsyev(c, c1, i, doubles, i1, i2, doubles1, i3)
  }

  def dgesvd(c: Char, c1: Char, i: Int, i1: Int, doubles: Array[Double],
             i2: Int, i3: Int, doubles1: Array[Double], i4: Int, doubles2: Array[Double], i5: Int, i6: Int, doubles3: Array[Double], i7: Int, i8: Int) = {
    assert(breeze.linalg.canLoadNativeBlas)
    NativeBlas.dgesvd(c, c1, i, i1, doubles, i2, i3, doubles1, i4, doubles2, i5, i6, doubles3, i7, i8)
  }

  def dpotrf(c: Char, i: Int, doubles: Array[Double], i1: Int, i2: Int) = {
    assert(breeze.linalg.canLoadNativeBlas)
    NativeBlas.dpotrf(c, i, doubles, i1, i2)
  }

  def dgemm(transa: Char, transb: Char, m: Int, n: Int, k: Int, alpha: Double, a: Array[Double], aIdx: Int, lda: Int, b: Array[Double], bIdx: Int, ldb: Int, beta: Double, c: Array[Double], cIdx: Int, ldc: Int) = {
    assert(breeze.linalg.canLoadNativeBlas)
    NativeBlas.dgemm(transa, transb, m, n, k, alpha, a, aIdx, lda, b, bIdx, ldb, beta, c, cIdx, ldc)
  }


}
