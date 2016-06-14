/*
 Copyright 2016 @author claydonkey (Anthony Campbell)

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

package breeze.linalg

import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{ getInstance => lapack }
import breeze.generic.UFunc
import breeze.linalg.hessenberg._
import breeze.linalg.householder._
import breeze.linalg.jacobi._
import breeze.math._
import breeze.numerics._
import scala.util.control.Breaks._
import breeze.linalg.Helper._
import DenseMatrix.canMapValues

/* returns tuple
      (y, z, Q, wR, wI, iLO2, iHI2) where
        y is  upper quasi-triangular matrix T from the Schur decomposition
      z  contains Q*Z. where
*/

object revertSchur extends UFunc {

  implicit object DMC_IMPL_DMC extends Impl2[DenseMatrix[Complex], DenseMatrix[Complex], DenseMatrix[Complex]] {
    def apply(M: DenseMatrix[Complex], T: DenseMatrix[Complex]): DenseMatrix[Complex] = {
      return T * upperTriangular(M) *  conj(T.t)
    }
  }
}

object schur extends UFunc {

  /* Schur decomposition  -computed by first reducing the
 * matrix to Hessenberg form using the class
 * HessenbergDecomposition. The Hessenberg matrix is then reduced
 * to triangular form by performing QR iterations with a single
 * shift. The cost of computing the Schur decomposition depends
 * on the number of iterations; as a rough guide, it may be taken
 * on the number of iterations; as a rough guide, it may be taken
 * to be \f$25n^3\f$ complex flops, or \f$10n^3\f$ complex flops
 * if \a computeU is false.*/

  /*
     *  Hessenberg decomposition of given matrix.
     *
     * The Hessenberg decomposition is computed by bringing the columns of the
     * matrix successively in the required form using Householder reflections
     * (see, e.g., Algorithm 7.4.2 in Golub \& Van Loan, <i>%Matrix
     * Computations</i>).
     */

  //return signature (Q,T,Householder tau, Householder  H)
  implicit object DMD_IMPL_SD extends Impl[DenseMatrix[Double], (DenseMatrix[Complex], DenseMatrix[Complex], DenseVector[Complex], DenseMatrix[Complex])] {
    def apply(M: DenseMatrix[Double]): (DenseMatrix[Complex], DenseMatrix[Complex], DenseVector[Complex], DenseMatrix[Complex]) = {

      new Schur[Double](M) {

        override val hess = hessenberg(M)
        val (s, z, y2, wR, wI, sLO, sHI) = getSchur(M) //LAPACK
        lazy override val T = DenseMatrix.tabulate[Complex](M.cols, M.rows)((i, j) => Complex(s(i, j), 0.0))
        lazy override val Q = hess._1 * DenseMatrix.tabulate[Complex](M.cols, M.rows)((i, j) => Complex(z(i, j), 0.0))
      }.decompose()
    }
  }

  implicit object DMD_IMPL_SI extends Impl[DenseMatrix[Int], (DenseMatrix[Complex], DenseMatrix[Complex], DenseVector[Complex], DenseMatrix[Complex])] {
    def apply(M: DenseMatrix[Int]): (DenseMatrix[Complex], DenseMatrix[Complex], DenseVector[Complex], DenseMatrix[Complex]) = {

      val MD = M.mapValues(_.toDouble)
      new Schur[Double](MD) {

        override val hess = hessenberg(MD)
        val (s, z, y2, wR, wI, sLO, sHI) = getSchur(MD) //LAPACK
        lazy override val T = DenseMatrix.tabulate[Complex](M.cols, M.rows)((i, j) => Complex(s(i, j), 0.0))
        lazy override val Q = hess._1 * DenseMatrix.tabulate[Complex](M.cols, M.rows)((i, j) => Complex(z(i, j), 0.0))
      }.decompose()
    }
  }

  implicit object DMCD_IMPL_SC extends Impl[DenseMatrix[Complex], (DenseMatrix[Complex], DenseMatrix[Complex], DenseVector[Complex], DenseMatrix[Complex])] {
    def apply(M: DenseMatrix[Complex]): (DenseMatrix[Complex], DenseMatrix[Complex], DenseVector[Complex], DenseMatrix[Complex]) = {

      new Schur[Complex](M) {

        override val hess = hessenberg(M)
        val m_maxIterationsPerRow = 30
        val maxIters = m_maxIterationsPerRow * hess._1.rows

        reduceToTriangularForm()
        /**
         * If matT(i+1,i) is neglegible in floating point arithmetic
         * compared to matT(i,i) and matT(j,j), then set it to zero and
         * return true, else return false.
         */
    private    def subdiagonalEntryIsNeglegible(i: Int) =
          {
            val d = norm1(T(i, i)) + norm1(T(i + 1, i + 1))
            val sd = norm1(T(i + 1, i))
            if (isMuchSmallerThan(sd, d)) {
              T(i + 1, i) = Complex(0.0, 0.0)
              true
            } else
              false
          }

        /** Compute the shift in the current QR iteration. */
      private  def computeShift(iu: Int, iter: Int) = {
          if (iter == 10 || iter == 20) {
            // exceptional shift, taken from http://www.netlib.org/eispack/comqr.f
            abs(T(iu, iu - 1).real) + abs(T(iu - 1, iu - 2).real)
          }
          // compute the shift as one of the eigenvalues of t, the 2x2
          // diagonal block on the bottom of the active submatrix

          var t = T((iu - 1) to iu, (iu - 1) to iu) // Complex(NormT, 0)
          val normt = Complex(sum(t.mapValues(abs(_))), 0)

          t = t / normt
          val b = t(0, 1) * t(1, 0)
          val c = t(0, 0) - t(1, 1)
          val disc = breeze.numerics.pow((c * c + 4.0 * b), 0.5)
          val det = (t(0, 0) * t(1, 1)) - b
          val trace = t(0, 0) + t(1, 1)
          var eival1 = (trace + disc) / 2.0
          var eival2 = (trace - disc) / 2.0

          if (norm1(eival1) > norm1(eival2))
            eival2 = det / eival1
          else
            eival1 = det / eival2
          // choose the eigenvalue closest to the bottom entry of the diagonal
          if (norm1(eival1 - t(1, 1)) < norm1(eival2 - t(1, 1)))
            normt * eival1
          else
            normt * eival2

        }
        /**A horrbly disFunctional implementation using breaks, iterations  and mutations BLEUURRGGH will change... **/
        // The matrix matT is divided in three parts.
        // Rows 0,...,il-1 are decoupled from the rest because matT(il,il-1) is zero.
        // Rows il,...,iu is the part we are working on (the active submatrix).
        // Rows iu+1,...,end are already brought in triangular form.
   private     def reduceToTriangularForm() = {

          var matnum = 0
          var matnum2 = 0
          var newrot = false
          var iu = T.cols - 1
          val maxIters = m_maxIterationsPerRow * T.rows
          var il = 0
          var iter = 0 // number of iterations we are working on the (iu,iu) element
          var totalIter = 0 // number of iterations for whole matrix

          breakable {
            while (true) {
              // find iu, the bottom row of the active submatrix
              breakable {
                while (iu > 0) {
                  if (subdiagonalEntryIsNeglegible(iu - 1)) {
                  }
                  if (!subdiagonalEntryIsNeglegible(iu - 1)) break
                  iter = 0
                  iu = iu - 1
                }

              }
              // if iu is zero then we are done; the whole matrix is triangularized

              if (iu == 0) break

              // if we spent too many iterations, we give up
              iter = iter + 1
              totalIter = totalIter + 1

              if (totalIter > maxIters) break

              // find il, the top row of the active submatrix
              il = iu - 1
              while (il > 0 && !subdiagonalEntryIsNeglegible(il - 1)) {
                il = il - 1
              }
              /* perform the QR step using Givens rotations. The first rotation
	   *creates a bulge; the (il+2,il) element becomes nonzero. This
	   *bulge is chased down to the bottom of the active submatrix.
	   */
              val givrot1 = makeGivens(T(il, il) - computeShift(iu, iter), T(il + 1, il))
              jacobi(T(il to il + 1, ::)).rotateL(givrot1)
              jacobi(T(0 to (min(il + 2, iu)), il to il + 1)) rotateR (givrot1)
              jacobi(Q(::, il to il + 1)) rotateR (givrot1)

              val idx: Int = 0

              for (idx <- ((il + 1) to iu - 1)) {
                val givrot2 = makeGivens(T(idx, idx - 1), T(idx + 1, idx - 1))
                T(idx, idx - 1) = givrot2.rot
                T(idx + 1, idx - 1) = Complex(0.0, 0.0)
                jacobi(T(idx to idx + 1, idx to T.cols - 1)) rotateL (givrot2)
                jacobi(T(0 to (min(idx + 2, iu)), idx to idx + 1)) rotateR (givrot2)
                jacobi(Q(::, idx to idx + 1)) rotateR (givrot2)
              }
            }
          }
        }
      }.decompose()
    }
  }
  abstract class Schur[T](val M: DenseMatrix[T]) {
    if (M.rows != M.cols)
      throw new MatrixNotSquareException

    val hess: (DenseMatrix[Complex], DenseMatrix[Complex], Householder)

    lazy val Q = hess._1
    lazy val T = hess._2


    def decompose() = (T, Q, hess._3.tau, hess._3.matrixH)
  }
  //tau =  the householder coeffs
  object getSchur extends Impl[DenseMatrix[Double], (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double], Array[Double], Array[Double], Int, Int)] {
    def apply(X: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double], Array[Double], Array[Double], Int, Int) = {

      val Y = X.copy

      val (h, iLO, iHI, tau) = getHessenbergLAPACK(Y)

      // y contains h the upper Hessenberg matrix H
      val M = h.rows
      val N = h.cols
      val Q = h.copy
      var wR = Array.fill[Double](N)(0)
      var wI = Array.fill[Double](N)(0)
      val (y2, iLO2, iHI2, tau2) = realOrthogonal(h, iLO, iHI, tau) // this is currently redundant  "V" in dhseqr does not work  . result to "I"??
      var z = y2.copy //  Q*Z.   is the orthogonal matrix generated by DORGHR   DOES NOT WORK (final value of Z)  =  (initial value of Z)*U
      var work = Array.ofDim[Double](N)
      var info = new intW(0)
      var workcnt = Array.ofDim[Double](1)

      lapack.dhseqr("S", "V", N, iLO, iHI, h.data, scala.math.max(1, N), wR, wI, z.data, scala.math.max(1, N), workcnt, -1, info)

      lapack.dhseqr(
        "S", /* 'E':  compute eigenvalues only;
         '	S':  compute eigenvalues and the Schur form */
        "V", /*COMPZ is CHARACTER*1
	'N':  no Schur vectors are computed;
	'I':  Z is initialized to the unit matrix and the matrix Z of Schur vectors of H is returned;
	'V':  Z must contain an orthogonal matrix Q on entry, and  the product Q*Z is returned. */
        N, /*N is INTEGER   The order of the matrix H.  N .GE. 0.*/
        iLO, /*ILO is INTEGER */
        iHI, /*IHI is INTEGER  */
        /*
	    It is assumed that H is already upper triangular in rows
	    and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
	    set by a previous call to DGEBAL, and then passed to ZGEHRD
	    when the matrix output by DGEBAL is reduced to Hessenberg
	    form. Otherwise ILO and IHI should be set to 1 and N
	    respectively.  If N.GT.0, then 1.LE.ILO.LE.IHI.LE.N.
	    If N = 0, then ILO = 1 and IHI = 0.
	*/
        h.data, /*
	     H (y2.data) is DOUBLE PRECISION array, dimension (LDH,N)
	    On entry, the upper Hessenberg matrix H.
	    On exit, if INFO = 0 and JOB = 'S', then H contains the
	    upper quasi-triangular matrix T from the Schur decomposition
	    (the Schur form); 2-by-2 diagonal blocks (corresponding to
	    complex conjugate pairs of eigenvalues) are returned in
	    standard form, with H(i,i) = H(i+1,i+1) and
	    H(i+1,i)*H(i,i+1).LT.0. If INFO = 0 and JOB = 'E', the
	    contents of H are unspecified on exit.  (The output value of
	    H when INFO.GT.0 is given under the description of INFO
	    below.)

	    Unlike earlier versions of DHSEQR, this subroutine may  explicitly H(i,j) = 0 for i.GT.j and j = 1, 2, ... ILO-1 or j = IHI+1, IHI+2, ... N.*/
        scala.math.max(1, N), /* LDH is INTEGER The leading dimension of the array H. LDH .GE. max(1,N).*/
        wR, /* WR is DOUBLE PRECISION array, dimension (N)*/
        wI, /* WL is DOUBLE PRECISION array, dimension (N)*/
        /*  The real and imaginary parts, respectively, of the computed
	    eigenvalues. If two eigenvalues are computed as a complex
	     conjugate pair, they are stored in consecutive elements of
	    WR and WI, say the i-th and (i+1)th, with WI(i) .GT. 0 and
	    WI(i+1) .LT. 0. If JOB = 'S', the eigenvalues are stored in
	    the same order as on the diagonal of the Schur form returned
	    in H, with WR(i) = H(i,i) and, if H(i:i+1,i:i+1) is a 2-by-2
	    diagonal block, WI(i) = sqrt(-H(i+1,i)*H(i,i+1)) and
	    WI(i+1) = -WI(i).
	   */
        z.data, /*LDZ (z.data)
	     Z is DOUBLE PRECISION array, dimension (LDZ,N)
	    If COMPZ = 'N', Z is not referenced.
	    If COMPZ = 'I', on entry Z need not be set and on exit,
	    if INFO = 0, Z contains the orthogonal matrix Z of the Schur
	     vectors of H.  If COMPZ = 'V', on entry Z must contain an
	    N-by-N matrix Q, which is assumed to be equal to the unit
	    matrix except for the submatrix Z(ILO:IHI,ILO:IHI). On exit,
	    if INFO = 0, Z contains Q*Z.
	    Normally Q is the orthogonal matrix generated by DORGHR
	    after the call to DGEHRD which formed the Hessenberg matrix
	    H. (The output value of Z when INFO.GT.0 is given under
	    the description of INFO below.)*/
        scala.math.max(1, N), /*WORK is DOUBLE PRECISION array, dimension (LWORK)
	    On exit, if INFO = 0, WORK(1) returns an estimate of the optimal value for LWORK.*/
        Array.ofDim[Double](workcnt(0).toInt), /*  WORK is DOUBLE PRECISION array, dimension (LWORK)
	    On exit, if INFO = 0, WORK(1) returns an estimate of the optimal value for LWORK.*/
        workcnt(0).toInt, /*LWORK is INTEGER
	    The dimension of the array WORK.  LWORK .GE. max(1,N)
	    is sufficient and delivers very good and sometimes
	    optimal performance.  However, LWORK as large as 11*N
	    may be required for optimal performance.  A workspace
	    query is recommended to determine the optimal workspace
	    size.

	    If LWORK = -1, then DHSEQR does a workspace query.
	    In this case, DHSEQR checks the input parameters and
	    estimates the optimal workspace size for the given
	     values of N, ILO and IHI.  The estimate is returned
	    in WORK(1).  No error message related to LWORK is
	    issued by XERBLA.  Neither H nor Z are accessed.*/
        info /*  INFO is INTEGER =  0:  successful exit
	    .LT. 0:  if INFO = -i, the i-th argument had an illegal value
	    .GT. 0:  if INFO = i, DHSEQR failed to compute all of the eigenvalues.  Elements 1:ilo-1 and i+1:n of WR
	    and WI contain those eigenvalues which have been successfully computed.  (Failures are rare
	    If INFO .GT. 0 and JOB = 'E', then on exit, the remaining unconverged eigenvalues are the
	    values of the upper Hessenberg matrix rows and columns ILO through INFO of the final, output value of H.
	    If INFO .GT. 0 and JOB   = 'S', then on exit
	    (*)  (initial value of H)*U  = U*(final value  where U is an orthogonal matrix.  The final value of H is upper Hessenberg and quasi-triangular in rows and columns INFO+1 through IHI.
	    If INFO .GT. 0 and COMPZ = 'V', then on exit  (final value of Z)  =  (initial value of Z)*U       where U is the orthogonal matrix in (*) (regardless of the value of JOB.)
	    If INFO .GT. 0 and COMPZ = 'I', then on exit (final value of Z)  = U where U is the orthogonal matrix in (*) (regardless of the value of JOB
	    If INFO .GT. 0 and COMPZ = 'N', then Z is not accessed.*/
      )  ;
      (h, z, Q, wR, wI, iLO2, iHI2)
    }

  }

}