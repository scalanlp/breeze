package breeze.linalg
/*
 Copyright 2016 @claydonkey (Anthony Campbell)

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
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{ getInstance => lapack }
import breeze.generic.UFunc
import breeze.math._
import breeze.linalg.householder._
import DenseMatrix.canMapValues
/*
 * [in]	N
          N is INTEGER
          The order of the matrix A.  N >= 0.
[in]	ILO
          ILO is INTEGER
[in]	IHI
          IHI is INTEGER

          It is assumed that A is already upper triangular in rows
          and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
          set by a previous call to DGEBAL; otherwise they should be
          set to 1 and N respectively. See Further Details.
          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
[in,out]	A
          A is DOUBLE PRECISION array, dimension (LDA,N)
          On entry, the N-by-N general matrix to be reduced.
          On exit, the upper triangle and the first subdiagonal of A
          are overwritten with the upper Hessenberg matrix H, and the
          elements below the first subdiagonal, with the array TAU,
          represent the orthogonal matrix Q as a product of elementary
          reflectors. See Further Details.
[in]	LDA
          LDA is INTEGER
          The leading dimension of the array A.  LDA >= max(1,N).
[out]	TAU
          TAU is DOUBLE PRECISION array, dimension (N-1)
          The scalar factors of the elementary reflectors (see Further
          Details). Elements 1:ILO-1 and IHI:N-1 of TAU are set to
          zero.
[out]	WORK
          WORK is DOUBLE PRECISION array, dimension (LWORK)
          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
[in]	LWORK
          LWORK is INTEGER
          The length of the array WORK.  LWORK >= max(1,N).
          For good performance, LWORK should generally be larger.

          If LWORK = -1, then a workspace query is assumed; the routine
          only calculates the optimal size of the WORK array, returns
          this value as the first entry of the WORK array, and no error
          message related to LWORK is issued by XERBLA.
[out]	INFO
          INFO is INTEGER
          = 0:  successful exit
          < 0:  if INFO = -i, the i-th argument had an illegal value.
	  The matrix Q is represented as a product of (ihi-ilo) elementary
  reflectors

     Q = H(ilo) H(ilo+1) . . . H(ihi-1).

  Each H(i) has the form

     H(i) = I - tau * v * v**T

  where tau is a real scalar, and v is a real vector with
  v(1:i) = 0, v(i+1) = 1 and v(ihi+1:n) = 0; v(i+2:ihi) is stored on
  exit in A(i+2:ihi,i), and tau in TAU(i).

  The contents of A are illustrated by the following example, with
  n = 7, ilo = 2 and ihi = 6:

  on entry,                        on exit,

  ( a   a   a   a   a   a   a )    (  a   a   h   h   h   h   a )
  (     a   a   a   a   a   a )    (      a   h   h   h   h   a )
  (     a   a   a   a   a   a )    (      h   h   h   h   h   h )
  (     a   a   a   a   a   a )    (      v2  h   h   h   h   h )
  (     a   a   a   a   a   a )    (      v2  v3  h   h   h   h )
  (     a   a   a   a   a   a )    (      v2  v3  v4  h   h   h )
  (                         a )    (                          a )

  where a denotes an element of the original matrix A, h denotes a
  modified element of the upper Hessenberg matrix H, and vi denotes an
  element of the vector defining H(i).

  This file is a slight modification of LAPACK-3.0's DGEHRD
  subroutine incorporating improvements proposed by Quintana-Orti and
  Van de Geijn (2006). (See DLAHR2.)

 */

/*
  PHP^(H)=A,

  A Hessenberg decomposition is a matrix decomposition of a matrix A into a unitary matrix P and a Hessenberg matrix H such that
  PHP^(H)=A,
  where P^(H) denotes the conjugate transpose.
  Hessenberg decomposition is implemented in the Wolfram Language as HessenbergDecomposition[m].
  Hessenberg decomposition is the first step in Schur decomposition. Hessenberg decomposition on an n×n matrix requires 14n^3/3 arithmetic operations.

  Computes the elementary reflector H such that:
  H * M = [ beta 0 ... 0]^T
  where the transformation H is:
  H = I - tau v v^*
  and the vector v is:
  v^T = [1 essential^T]

  Householder obj variables:
  essential the essential part of the vector  v
  House.tau  the scaling factor of the Householder transformation
  House.beta the result of H * M
 */

//Both DenseVector [Double] and [Int] functions  need to be simplified to return real matrices only...
object hessenberg extends UFunc {

  implicit object DMC_IMPL_HeCHo extends Impl[DenseMatrix[Complex], (DenseMatrix[Complex], DenseMatrix[Complex], Householder)] {
    def apply(M: DenseMatrix[Complex]): (DenseMatrix[Complex], DenseMatrix[Complex], Householder) = {
      new Hessenberg[Complex](M) {
        override val House = new Householder(M.copy).generateFullHouseholder()
        //LAPACK misses this step
      }.decompose()
    }
  }

  implicit object DMD_IMPL_HeDHo extends Impl[DenseMatrix[Double], (DenseMatrix[Complex], DenseMatrix[Complex], Householder)] {
    def apply(M: DenseMatrix[Double]): (DenseMatrix[Complex], DenseMatrix[Complex], Householder) = {
      new Hessenberg[Double](M) {
        override val House = {
          val (h, hLO, hHI, tau) = getHessenbergLAPACK(M)
          new Householder(h.mapValues(Complex(_, 0.0)), DenseVector.tabulate[Complex](M.cols - 1)(i => Complex(tau(i), 0.0)))
        }
      }.decompose()
    }
  }

  implicit object DMD_IMPL_DMI_DMI_Ho extends Impl[DenseMatrix[Int], (DenseMatrix[Complex], DenseMatrix[Complex], Householder)] {
    def apply(M: DenseMatrix[Int]): (DenseMatrix[Complex], DenseMatrix[Complex], Householder) = {
      new Hessenberg[Int](M) {
        override val House = {
          val (h, hLO, hHI, tau) = getHessenbergLAPACK(M.mapValues(_.toDouble))
          new Householder(h.mapValues(Complex(_, 0.0)), DenseVector.tabulate[Complex](M.cols - 1)(i => Complex(tau(i), 0.0)))
        }
      }.decompose()
    }
  }

  abstract class Hessenberg[T](M: DenseMatrix[T]) {

    val House: Householder
    def decompose() = (P, H, House)
    /*  4 x 4 example of the form
     *  1    0    0     0   ^  ------- order
     *   0    1    0    0   v
     *   0    0     x    x
     *   0    0     x    x
     */
    def P: DenseMatrix[Complex] = householder.householderTransformation(House.matrixH.mapValues(_.real), House.coeffs.mapValues(_.real), 1).mapValues(Complex(_, 0.0))
    /*  4 x 4 example
     *   x    x    x     x
     *   x    x    x    x
     *   0    x     x    x
     *   0    0     x    x
     */
    def H: DenseMatrix[Complex] = DenseMatrix.tabulate(House.matrixH.rows, House.matrixH.rows)((i, j) => if (j >= i - 1) House.matrixH(i, j) else Complex(0, 0))
  }
  def getHessenbergLAPACK(X: DenseMatrix[Double]): (DenseMatrix[Double], Int, Int, Array[Double]) = {

    val M = X.rows
    val N = X.cols
    val y = X.copy
    var iLO = new intW(0)
    var iHI = new intW(0)
    var iScale = Array.ofDim[Double](N)
    var tau = Array.ofDim[Double](N)
    var info = new intW(0)

    lapack.dgebal(
      "P", //'B':  both permute and scale. 'P':  permute only;
      N, //N is INTEGER     The order of the matrix A.  N >= 0

      y.data, /*	  A is DOUBLE array, dimension (LDA,N)
		 On entry, the input matrix A.
		On exit,  A is overwritten by the balanced matrix.
		If JOB = 'N', A is not referenced.         See Further Details. */
      scala.math.max(1, M), /* LDA is INTEGER       The leading dimension of the array A.  LDA >= max(1,N). */
      iLO,
      iHI,
      iScale,
      info
    )

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    var workcnt = Array.ofDim[Double](1)

    lapack.dgehrd(N, iLO.`val`, iHI.`val`, y.data, scala.math.max(1, N), tau, workcnt, -1, info)
    lapack.dgehrd(N, iLO.`val`, iHI.`val`, y.data, scala.math.max(1, N), tau, Array.ofDim[Double](workcnt(0).toInt), workcnt(0).toInt, info)

    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    (y, iLO.`val`, iHI.`val`, tau)
  }

}