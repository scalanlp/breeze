package breeze.linalg

import breeze.generic.UFunc
import org.netlib.util.intW
import dev.ludovic.netlib.LAPACK.{getInstance => lapack}
import scalaxy.debug._

/**
 * Eigenvalue decomposition (right eigenvectors)
 *
 * This function returns the real and imaginary parts of the eigenvalues,
 * and the corresponding eigenvectors.  For most (?) interesting matrices,
 * the imaginary part of all eigenvalues will be zero (and the corresponding
 * eigenvectors will be real).  Any complex eigenvalues will appear in
 * complex-conjugate pairs, and the real and imaginary components of the
 * eigenvector for each pair will be in the corresponding columns of the
 * eigenvector matrix.  Take the complex conjugate to find the second
 * eigenvector.
 *
 * Based on EVD.java from MTJ 0.9.12
 */
object eig extends UFunc {

  // TODO: probably we should just return an eigenValues: DV[Complex] ?
  case class Eig[V, M](eigenvalues: V, eigenvaluesComplex: V, eigenvectors: M)
  type DenseEig = Eig[DenseVector[Double], DenseMatrix[Double]]

  implicit object Eig_DM_Impl extends Impl[DenseMatrix[Double], DenseEig] {
    def apply(m: DenseMatrix[Double]): DenseEig = {
      requireNonEmptyMatrix(m)
      requireSquareMatrix(m)
      require(!m.valuesIterator.exists(_.isNaN))

      val n = m.rows

      // Allocate space for the decomposition
      val Wr = DenseVector.zeros[Double](n)
      val Wi = DenseVector.zeros[Double](n)

      val Vr = DenseMatrix.zeros[Double](n, n)
      val Vl = DenseMatrix.zeros[Double](n, n)

      // Find the needed workspace
      val worksize = Array.ofDim[Double](1)
      val info = new intW(0)

      lapack.dgeev(
        "N",
        "V",
        n,
        Array.empty[Double],
        scala.math.max(1, n),
        Array.empty[Double],
        Array.empty[Double],
        Array.empty[Double],
        scala.math.max(1, n),
        Array.empty[Double],
        scala.math.max(1, n),
        worksize,
        -1,
        info
      )

      // Allocate the workspace
      val lwork: Int =
        if (info.`val` != 0)
          scala.math.max(1, 4 * n)
        else
          scala.math.max(1, worksize(0).toInt)

      val work = Array.ofDim[Double](lwork)

      // Factor it!

      val A = DenseMatrix.zeros[Double](n, n)
      A := m
      lapack.dgeev(
        "N",
        "V",
        n,
        A.data,
        scala.math.max(1, n),
        Wr.data,
        Wi.data,
        Vl.data,
        scala.math.max(1, n),
        Vr.data,
        scala.math.max(1, n),
        work,
        work.length,
        info)

      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()

      Eig(Wr, Wi, Vr)
    }
  }

}

/**
 * Computes all eigenvalues (and optionally right eigenvectors) of the given
 * real symmetric matrix X.
 */
object eigSym extends UFunc {
  case class EigSym[V, M](eigenvalues: V, eigenvectors: M)
  type DenseEigSym = EigSym[DenseVector[Double], DenseMatrix[Double]]
  implicit object EigSym_DM_Impl extends Impl[DenseMatrix[Double], DenseEigSym] {
    def apply(X: DenseMatrix[Double]): DenseEigSym = {
      doEigSym(X, true) match {
        case (ev, Some(rev)) => EigSym(ev, rev)
        case _ => throw new RuntimeException("Shouldn't be here!")
      }

    }
  }

  object justEigenvalues extends UFunc {
    implicit object EigSym_DM_Impl extends Impl[DenseMatrix[Double], DenseVector[Double]] {
      def apply(X: DenseMatrix[Double]): DenseVector[Double] = {
        doEigSym(X, false)._1
      }
    }

  }

  private def doEigSym(
      X: Matrix[Double],
      rightEigenvectors: Boolean): (DenseVector[Double], Option[DenseMatrix[Double]]) = {
    requireNonEmptyMatrix(X)

    // As LAPACK doesn't check if the given matrix is in fact symmetric,
    // we have to do it here (or get rid of this time-waster as long as
    // the caller of this function is clearly aware that only the lower
    // triangular portion of the given matrix is used and there is no
    // check for symmetry).
    requireSymmetricMatrix(X)

    // Copy the lower triangular part of X. LAPACK will store the result in A.
    val A = lowerTriangular(X)

    val N = X.rows
    val evs = DenseVector.zeros[Double](N)
    val lwork = scala.math.max(1, 3 * N - 1)
    val work = Array.ofDim[Double](lwork)
    val info = new intW(0)
    lapack.dsyev(
      if (rightEigenvectors) "V" else "N" /* eigenvalues N, eigenvalues & eigenvectors "V" */,
      "L" /* lower triangular */,
      N /* number of rows */,
      A.data,
      scala.math.max(1, N) /* LDA */,
      evs.data,
      work /* workspace */,
      lwork /* workspace size */,
      info
    )
    // A value of info.`val` < 0 would tell us that the i-th argument
    // of the call to dsyev was erroneous (where i == |info.`val`|).
    assert(info.`val` >= 0)

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)

    (evs, if (rightEigenvectors) Some(A) else None)
  }
}
