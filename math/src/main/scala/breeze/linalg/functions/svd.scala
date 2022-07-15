package breeze.linalg

import breeze.generic.UFunc
import dev.ludovic.netlib.lapack.LAPACK.{getInstance => lapack}
import dev.ludovic.netlib.arpack.ARPACK
import org.netlib.util.intW
import org.netlib.util.doubleW
import breeze.linalg.operators.OpMulMatrix
import breeze.linalg.support.CanTranspose
import breeze.macros.cforRange

//  Options fot the singular value decomposition (SVD) of a real M-by-N matrix
sealed private[this] abstract class SVDMode(val JOBZ: String)
private[this] case object CompleteSVD extends SVDMode("A") // all M columns of U and all N rows of V**T are returned in the arrays U and VT
private[this] case object ReducedSVD extends SVDMode("S") // the first min(M,N) columns of U and the first min(M,N) rows of V**T are returned in the arrays U and VT

/**
 * Computes the SVD of a M-by-N matrix
 * Returns an M-by-M matrix U, a vector of singular values, and a N-by-N matrix V'
 */
object svd extends UFunc {

  case class SVD[M, V](leftVectors: M, singularValues: V, rightVectors: M) {
    def U: M = leftVectors
    def ∑ : V = singularValues
    def S = ∑
    def Vt: M = rightVectors
  }

  type DenseSVD = SVD[DenseMatrix[Double], DenseVector[Double]]
  type SDenseSVD = SVD[DenseMatrix[Float], DenseVector[Float]]

  // implementations

  implicit object Svd_DM_Impl extends Impl[DenseMatrix[Double], DenseSVD] {
    def apply(mat: DenseMatrix[Double]): DenseSVD = doSVD_Double(mat)(mode = CompleteSVD)
  }

  implicit object Svd_DM_Impl_Float extends Impl[DenseMatrix[Float], SDenseSVD] {
    def apply(mat: DenseMatrix[Float]): SDenseSVD = doSVD_Float(mat)(mode = CompleteSVD)
  }

  /**
   * Option for computing part of the M-by-N matrix U:
   *       The first min(M,N) columns of U and the first min(M,N)
   *       rows of V**T are returned in the arrays U and VT;
   */
  object reduced extends UFunc {
    implicit object reduced_Svd_DM_Impl extends Impl[DenseMatrix[Double], DenseSVD] {
      def apply(mat: DenseMatrix[Double]): DenseSVD = doSVD_Double(mat)(mode = ReducedSVD)
    }

    implicit object reduced_Svd_DM_Impl_Float extends Impl[DenseMatrix[Float], SDenseSVD] {
      def apply(mat: DenseMatrix[Float]): SDenseSVD = doSVD_Float(mat)(mode = ReducedSVD)
    }
  }

  /**
   * Computes the singular value decomposition (SVD) of a real M-by-N matrix
   *
   * @param mat Real M-by-N matrix (DOUBLE PRECISION)
   * @param mode Mode of the SVD
   * @return The singular value decomposition (SVD) with the singular values,
   *         the left and right singular vectors (DOUBLE PRECISION)
   */
  private def doSVD_Double(mat: DenseMatrix[Double])(mode: SVDMode): DenseSVD = {
    requireNonEmptyMatrix(mat)

    val m = mat.rows
    val n = mat.cols
    val S = DenseVector.zeros[Double](m.min(n))
    val U = mode match {
      case CompleteSVD => DenseMatrix.zeros[Double](m, m)
      case ReducedSVD => DenseMatrix.zeros[Double](m, m.min(n))
    }
    val Vt = mode match {
      case CompleteSVD => DenseMatrix.zeros[Double](n, n)
      case ReducedSVD => DenseMatrix.zeros[Double](m.min(n), n)
    }
    val iwork = new Array[Int](8 * (m.min(n)))
    val workSize = (3L
      * scala.math.min(m, n)
      * scala.math.min(m, n)
      + scala.math.max(
        scala.math.max(m, n),
        4L * scala.math.min(m, n)
          * scala.math.min(m, n) + 4L * scala.math.min(m, n)))
    if (workSize >= Int.MaxValue) {
      throw new RuntimeException(
        "The param k and numFeatures is too large for SVD computation. " +
          "Try reducing the parameter k for PCA, or reduce the input feature " +
          "vector dimension to make this tractable.")
    }
    val work = new Array[Double](workSize.toInt)
    val info = new intW(0)
    val cm = copy(mat)

    val LDVT = mode match {
      case CompleteSVD => scala.math.max(1, n)
      case ReducedSVD => m.min(n)
    }

    lapack.dgesdd(
      mode.JOBZ,
      m,
      n,
      cm.data,
      scala.math.max(1, m),
      S.data,
      U.data,
      scala.math.max(1, m),
      Vt.data,
      LDVT,
      work,
      work.length,
      iwork,
      info)

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    SVD(U, S, Vt)
  }

  /**
   * Computes the singular value decomposition (SVD) of a real M-by-N matrix
   * @param mat Real M-by-N matrix (SINGLE PRECISION)
   * @param mode Mode of the SVD
   * @return The singular value decomposition (SVD) with the singular values,
   *         the left and right singular vectors (SINGLE PRECISION)
   */
  private def doSVD_Float(mat: DenseMatrix[Float])(mode: SVDMode): SDenseSVD = {
    requireNonEmptyMatrix(mat)

    val m = mat.rows
    val n = mat.cols
    val S = DenseVector.zeros[Float](m.min(n))
    val U = mode.JOBZ match {
      case "A" => DenseMatrix.zeros[Float](m, m)
      case "S" => DenseMatrix.zeros[Float](m, m.min(n))
    }
    val Vt = mode.JOBZ match {
      case "A" => DenseMatrix.zeros[Float](n, n)
      case "S" => DenseMatrix.zeros[Float](m.min(n), n)
    }
    val iwork = new Array[Int](8 * (m.min(n)))
    val workSize = (3
      * scala.math.min(m, n)
      * scala.math.min(m, n)
      + scala.math.max(
        scala.math.max(m, n),
        4 * scala.math.min(m, n)
          * scala.math.min(m, n) + 4 * scala.math.min(m, n)))
    val work = new Array[Float](workSize)
    val info = new intW(0)
    val cm = copy(mat)

    val LDVT = mode.JOBZ match {
      case "A" => scala.math.max(1, n)
      case "S" => m.min(n)
    }

    lapack.sgesdd(
      mode.JOBZ,
      m,
      n,
      cm.data,
      scala.math.max(1, m),
      S.data,
      U.data,
      scala.math.max(1, m),
      Vt.data,
      LDVT,
      work,
      work.length,
      iwork,
      info)

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    SVD(U, S, Vt)
  }

  type OpMulMatrix_Mat_DV_eq_DV[Mat] = OpMulMatrix.Impl2[Mat, DenseVector[Double], DenseVector[Double]]

  /**
   * Implementation of svds for a sparse matrix. The caller provides two operations: mul - matrix
   * multiplies a DenseVector, and trans - matrix transpose.
   *
   * @param mul Operation that multiples a matrix with a DenseVector. Example:
   * {{{
   * implicit object Op_Mul_Mat_V extends OpMulMatrixDenseVector[UserMatrixType] {
   *   def apply(mt: UserMatrixType, iv: DenseVector[Double]) = {
   *     // return another DenseVector[Double] = mt * iv
   *   }
   * }
   * }}}
   * @param trans Operator for transposing the matrix. Example:
   * {{{
   * implicit object Op_Mat_Trans extends CanTranspose[UserMatrixType, UserMatrixTypeTranspose] {
   *   def apply(mt: UserMatrixType) = {
   *     // return a UserMatrixTypeTranspose which is the transpose of mt
   *   }
   * }
   * }}}
   * @param mulTrans Operation that multiples a transposed matrix with a DenseVector. Example:
   * {{{
   * // if UserMatrixType and UserMatrixTypeTranspose are actually the same type, you do not need this
   * implicit object Op_Mul_Mat_V extends OpMulMatrixDenseVector[UserMatrixTypeTranspose] {
   *   def apply(mtTrans: UserMatrixTypeTranspose, iv: DenseVector[Double]) = {
   *     // return another DenseVector[Double] = mtTrans * iv
   *   }
   * }
   * }}}
   * @tparam Mat Type of the input matrix of size n*m.
   * @tparam MatTranspose Type of the transpose of input matrix of size m*n.
   * @return Left singular vectors matrix of size n*k, singular value vector of length k, and
   *         transpose of right singular vectors matrix of size k*m.
   */
  implicit def Svd_Sparse_Impl[Mat, MatTranspose](
                                                   implicit mul: OpMulMatrix_Mat_DV_eq_DV[Mat],
                                                   trans: CanTranspose[Mat, MatTranspose],
                                                   mulTrans: OpMulMatrix_Mat_DV_eq_DV[MatTranspose],
                                                   dimImpl: dim.Impl[Mat, (Int, Int)]): Impl3[Mat, Int, Double, DenseSVD] = {

    class Svd_Sparse_Impl_Instance extends Impl3[Mat, Int, Double, DenseSVD] {
      val arpack = ARPACK.getInstance()

      def av(
          mat: Mat,
          matTrans: MatTranspose,
          n: Int,
          k: Int,
          work: Array[Double],
          input_offset: Int,
          output_offset: Int): Unit = {
        val w = DenseVector(work)
        val x = w(input_offset until input_offset + n)
        val y = w(output_offset until output_offset + n)

        val z = mulTrans(matTrans, x)
        if (z.length <= k)
          throw new IllegalArgumentException(
            "The number of rows or columns " +
              "should be bigger than k.")
        y := mul(mat, z)
      }

      /**
       * Generic svds computation. This function computes the largest k singular values and
       * corresponding singular vectors.
       *
       * @param mt Input matrix of size n x m. Usually the caller should make sure n < m so that
       *           less working memory is required by ARPACK.
       * @param k Number of desired singular values.
       * @param tol Tolerance of the svd computation.
       * @return Left singular vectors matrix of size n*k, singular value vector of length k, and
       *         transpose of right singular vectors matrix of size k*m.
       */
      def apply(mt: Mat, k: Int, tol: Double): DenseSVD = {
        val n = dim(mt)._1
        if (n <= k)
          throw new IllegalArgumentException("The number of rows or columns should be bigger than k.")

        val mtTrans = trans.apply(mt)

        val tolW = new doubleW(tol)

        val nev = new intW(k)
        val ncv = scala.math.min(2 * k, n)

        val bmat = "I"
        val which = "LM"

        var iparam = new Array[Int](11)
        iparam(0) = 1
        iparam(2) = 300
        iparam(6) = 1

        var ido = new intW(0)
        var info = new intW(0)
        var resid: Array[Double] = new Array[Double](n)
        var v = new Array[Double](n * ncv)
        var workd = new Array[Double](3 * n)
        var workl = new Array[Double](ncv * (ncv + 8))
        var ipntr = new Array[Int](11)

        arpack.dsaupd(
          ido,
          bmat,
          n,
          which,
          nev.`val`,
          tolW,
          resid,
          ncv,
          v,
          n,
          iparam,
          ipntr,
          workd,
          workl,
          workl.length,
          info)

        while (ido.`val` != 99) {
          if (ido.`val` != -1 && ido.`val` != 1)
            throw new IllegalStateException("ido = " + ido.`val`)
          av(mt, mtTrans, n, k, workd, ipntr(0) - 1, ipntr(1) - 1)
          arpack.dsaupd(
            ido,
            bmat,
            n,
            which,
            nev.`val`,
            tolW,
            resid,
            ncv,
            v,
            n,
            iparam,
            ipntr,
            workd,
            workl,
            workl.length,
            info)
        }

        if (info.`val` != 0) throw new IllegalStateException("info = " + info.`val`)

        val d = new Array[Double](nev.`val`)
        val select = new Array[Boolean](ncv)
        val z = java.util.Arrays.copyOfRange(v, 0, nev.`val` * n)

        arpack.dseupd(
          true,
          "A",
          select,
          d,
          z,
          n,
          0.0,
          bmat,
          n,
          which,
          nev,
          tol,
          resid,
          ncv,
          v,
          n,
          iparam,
          ipntr,
          workd,
          workl,
          workl.length,
          info)

        val computed = iparam(4)
        val eigenVectors = new DenseVector(z)

        var mp = new Array[(Double, DenseVector[Double])](computed)

        cforRange(0 until computed) { i =>
          val eigenVal = d(i)
          if (eigenVal < 0.0)
            throw new IllegalStateException(
              "encountered negative eigenvalue, " +
                "please make sure your multiplication operators are applied to the same matrix.")
          val eigenVec = eigenVectors(i * n until i * n + n)
          mp(i) = (scala.math.sqrt(eigenVal), eigenVec)
        }

        mp = mp.sortBy(-1 * _._1)
        val sp = mp.map(_._1)

        val s = DenseVector(sp.toArray)
        val siMatrix: DenseMatrix[Double] = diag(DenseVector(sp.map(u => 1 / u).toArray))

        val va = mp.map { case (ek, ev) => ev }
        val uOutput = DenseMatrix(va.map(r => r.toArray).toSeq: _*).t
        val vtOutput = siMatrix * DenseMatrix(va.map(r => mulTrans(mtTrans, r).toArray).toSeq: _*)
        SVD(uOutput, s, vtOutput)
      }
    }

    new Svd_Sparse_Impl_Instance
  }

  implicit object Svd_SM_Impl extends Impl2[CSCMatrix[Double], Int, DenseSVD] {

    /**
     * Svds for CSCMatrix[Double]. This function computes the largest k singular values and
     * corresponding singular vectors. Default tolerance is set to 1e-6.
     *
     * @param mt Input matrix of type CSCMatrix[Double].
     * @param k Number of desired singular values.
     * @return Left singular vectors matrix of size n*k, singular value vector of length k, and
     *         transpose of right singular vectors matrix of size k*m.
     */
    def apply(mt: CSCMatrix[Double], k: Int): DenseSVD = {
      val tol = 1e-6
      if (k >= mt.cols || k >= mt.rows) {
        throw new IllegalArgumentException(
          "The desired number of singular values is greater " +
            "than or equal to min(mt.cols, mt.rows). Please use the full svd.")
      }

      val svdImpl = svd.Svd_Sparse_Impl[CSCMatrix[Double], CSCMatrix[Double]]
      val isSlimMatrix = mt.rows > mt.cols
      val SVD(u, s, vt) =
        if (isSlimMatrix)
          svdImpl(mt.t, k, tol)
        else
          svdImpl(mt, k, tol)

      if (isSlimMatrix) SVD(vt.t, s, u.t) else SVD(u, s, vt)
    }
  }
}
