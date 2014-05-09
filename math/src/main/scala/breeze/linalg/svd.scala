package breeze.linalg

import breeze.generic.UFunc
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}
import com.github.fommil.netlib.ARPACK
import org.netlib.util.intW
import org.netlib.util.doubleW
import breeze.linalg.operators.OpMulMatrix
import breeze.linalg.support.CanTranspose

/**
  * Computes the SVD of a m by n matrix
  * Returns an m*m matrix U, a vector of singular values, and a n*n matrix V'
  */
object svd extends UFunc {
  implicit object Svd_DM_Impl extends Impl[DenseMatrix[Double], (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double])] {
    def apply(mat: DenseMatrix[Double]): (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double]) = {
      requireNonEmptyMatrix(mat)

      val m = mat.rows
      val n = mat.cols
      val S = DenseVector.zeros[Double](m min n)
      val U = DenseMatrix.zeros[Double](m,m)
      val Vt = DenseMatrix.zeros[Double](n,n)
      val iwork = new Array[Int](8 * (m min n) )
      val workSize = ( 3
        * scala.math.min(m, n)
        * scala.math.min(m, n)
        + scala.math.max(scala.math.max(m, n), 4 * scala.math.min(m, n)
          * scala.math.min(m, n) + 4 * scala.math.min(m, n))
      )
      val work = new Array[Double](workSize)
      val info = new intW(0)
      val cm = copy(mat)

      lapack.dgesdd(
        "A", m, n,
        cm.data, scala.math.max(1,m),
        S.data, U.data, scala.math.max(1,m),
        Vt.data, scala.math.max(1,n),
        work,work.length,iwork, info)

      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()

      (U,S,Vt)
    }
  }

  type OpMulMatrixDenseVector[Mat] = OpMulMatrix.Impl2[Mat, DenseVector[Double], DenseVector[Double]]

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
  implicit def Svd_Sparse_Impl[Mat, MatTranspose](implicit mul: OpMulMatrixDenseVector[Mat],
                                                  trans: CanTranspose[Mat, MatTranspose],
                                                  mulTrans: OpMulMatrixDenseVector[MatTranspose])
    :Impl4[Mat, Int, Int, Double, (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double])] = {

    class Svd_Sparse_Impl_Instance extends Impl4[Mat, Int, Int, Double, (DenseMatrix[Double],
        DenseVector[Double], DenseMatrix[Double])] {
      val arpack  = ARPACK.getInstance()

      def av( mat: Mat, matTrans: MatTranspose, n: Int, k: Int, work:Array[Double], input_offset:Int, output_offset:Int) {
        val w = DenseVector(work)
        val x = w(input_offset until input_offset+n)
        val y = w(output_offset until output_offset+n)

        val z = mulTrans(matTrans, x)
        if (z.length <= k) throw new IllegalArgumentException("The number of rows or columns " +
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
        * @param n Number of rows of the input matrix.
        * @param tol Tolerance of the svd computation.
        * @return Left singular vectors matrix of size n*k, singular value vector of length k, and
        *         transpose of right singular vectors matrix of size k*m.
        */
      def apply(mt: Mat, k: Int, n: Int, tol: Double): (DenseMatrix[Double], DenseVector[Double],
          DenseMatrix[Double]) = {
        if (n <= k) throw new IllegalArgumentException("The number of rows or columns should be " +
            "bigger than k.")

        val mtTrans = trans.apply(mt)

        val tolW = new doubleW(tol)

        val nev = new intW(k)
        val ncv = scala.math.min(2*k,n)

        val bmat = "I"
        val which = "LM"

        var iparam = new Array[Int](11)
        iparam(0) = 1
        iparam(2) = 300
        iparam(6) = 1

        var ido = new intW(0)
        var info = new intW(0)
        var resid:Array[Double] = new Array[Double](n)
        var v = new Array[Double](n*ncv)
        var workd = new Array[Double](3*n)
        var workl = new Array[Double](ncv*(ncv+8))
        var ipntr = new Array[Int](11)

        arpack.dsaupd(ido, bmat, n, which, nev.`val`, tolW, resid, ncv, v, n, iparam, ipntr, workd,
          workl, workl.length, info)

        while(ido.`val` !=99) {
          if (ido.`val` != -1 && ido.`val` != 1)
            throw new IllegalStateException("ido = " + ido.`val`)
          av(mt, mtTrans, n, k, workd, ipntr(0) - 1, ipntr(1) - 1)
          arpack.dsaupd(ido, bmat, n, which, nev.`val`, tolW, resid, ncv, v, n, iparam, ipntr,
            workd, workl, workl.length, info)
        }

        if (info.`val` != 0) throw new IllegalStateException("info = " + info.`val`)

        val d = new Array[Double](nev.`val`)
        val select = new Array[Boolean](ncv)
        val z = java.util.Arrays.copyOfRange(v, 0, nev.`val` * n)

        arpack.dseupd(true, "A", select, d, z, n, 0.0, bmat, n, which, nev, tol, resid, ncv, v, n,
          iparam, ipntr, workd, workl, workl.length, info)

        val computed = iparam(4)
        val eigenVectors = new DenseVector(z)

        var mp = new Array[(Double,DenseVector[Double])](computed)

        for( i <- 0 until computed){
          val eigenVal = d(i)
          if (eigenVal < 0.0) throw new IllegalStateException("encountered negative eigenvalue, " +
              "please make sure your multiplication operators are applied to the same matrix.")
          val eigenVec = eigenVectors(i*n until i*n + n)
          mp(i) = (scala.math.sqrt(eigenVal),eigenVec)
        }

        mp = mp.sortBy(-1*_._1)
        val sp = mp.map(_._1)

        val s = DenseVector(sp.toArray)
        val siMatrix: DenseMatrix[Double] = diag(DenseVector(sp.map(u => 1/u).toArray))

        val va = mp.map{case(ek,ev) => ev}
        val uOutput = DenseMatrix(va.map(r => r.toArray).toSeq:_*).t
        val vtOutput = siMatrix * DenseMatrix(va.map(r => mulTrans(mtTrans, r).toArray).toSeq:_*)
        (uOutput,s,vtOutput)
      }
    }

    new Svd_Sparse_Impl_Instance
  }

/*
 returns
 u : shape=(M, k)
 Unitary matrix having left singular vectors as columns.

 s : Dense vector of singular values.

 vt : shape=(k, N)
 Unitary matrix having right singular vectors as rows.
*/
  implicit object Svd_SM_Impl extends
    Impl2[CSCMatrix[Double],Int, (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double])] {

    /**
      * Svds for CSCMatrix[Double]. This function computes the largest k singular values and
      * corresponding singular vectors. Default tolerance is set to 1e-6.
      *
      * @param mt Input matrix of type CSCMatrix[Double].
      * @param k Number of desired singular values.
      * @return Left singular vectors matrix of size n*k, singular value vector of length k, and
      *         transpose of right singular vectors matrix of size k*m.
      */
    def apply(mt: CSCMatrix[Double], k: Int):
        (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double]) = {
      val tol = 1e-6
      if (k >= mt.cols || k >= mt.rows) {
        throw new IllegalArgumentException("The desired number of singular values is greater " +
            "than or equal to min(mt.cols, mt.rows). Please use the full svd.")
      }

      val svdImpl = svd.Svd_Sparse_Impl[CSCMatrix[Double],CSCMatrix[Double]]
      val isSlimMatrix = mt.rows > mt.cols
      val (u, s, vt) = if (isSlimMatrix)
          svdImpl(mt.t, k, mt.cols, tol)
        else
          svdImpl(mt, k, mt.rows, tol)

      if (isSlimMatrix) (vt.t, s, u.t) else (u, s, vt)
    }
  }
}
