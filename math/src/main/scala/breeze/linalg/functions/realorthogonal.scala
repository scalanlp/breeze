package breeze.linalg

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

import breeze.macros.expand
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{ getInstance => lapack }
import breeze.generic.UFunc
import breeze.linalg.operators.{ OpSolveMatrixBy, OpMulMatrix }
import breeze.linalg.support.CanTranspose
/*
 DORGHR generates a real orthogonal matrix Q which is defined as the
 product of IHI-ILO elementary reflectors of order N, as returned by
 DGEHRD:

 Q = H(ilo) H(ilo+1) . . . H(ihi-1).
Parameters
[in]	N
          N is INTEGER
          The order of the matrix Q. N >= 0.
[in]	ILO
          ILO is INTEGER
[in]	IHI
          IHI is INTEGER

          ILO and IHI must have the same values as in the previous call
          of DGEHRD. Q is equal to the unit matrix except in the
          submatrix Q(ilo+1:ihi,ilo+1:ihi).
          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
[in,out]	A
          A is DOUBLE PRECISION array, dimension (LDA,N)
          On entry, the vectors which define the elementary reflectors,
          as returned by DGEHRD.
          On exit, the N-by-N orthogonal matrix Q.
[in]	LDA
          LDA is INTEGER
          The leading dimension of the array A. LDA >= max(1,N).
[in]	TAU
          TAU is DOUBLE PRECISION array, dimension (N-1)
          TAU(i) must contain the scalar factor of the elementary
          reflector H(i), as returned by DGEHRD.
[out]	WORK
          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK))
          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
[in]	LWORK
          LWORK is INTEGER
          The dimension of the array WORK. LWORK >= IHI-ILO.
          For optimum performance LWORK >= (IHI-ILO)*NB, where NB is
          the optimal blocksize.

          If LWORK = -1, then a workspace query is assumed; the routine
          only calculates the optimal size of the WORK array, returns
          this value as the first entry of the WORK array, and no error
          message related to LWORK is issued by XERBLA.
[out]	INFO
          INFO is INTEGER
          = 0:  successful exit
          < 0:  if INFO = -i, the i-th argument had an illegal value
*/

object realorthogonal extends UFunc {
  implicit object RealOrthogonal_DMIIAD_Impl_DM_Int_Int_AD extends Impl4[DenseMatrix[Double],Int, Int, Array[Double], (DenseMatrix[Double], Int, Int, Array[Double])] {
    def apply(X: DenseMatrix[Double],   LO : Int , Hi : Int, tau: Array[Double]) : (DenseMatrix[Double], Int, Int, Array[Double]) = {

  var iLO = new intW(LO)
         var iHI = new intW(Hi)

      val M = X.rows
      val N = X.cols
      val y = X.copy

      var iScale = Array.ofDim[Double](N)
      var tau = Array.ofDim[Double](N)
      var info = new intW(0)
      var workcnt = Array.ofDim[Double](1)

      lapack.dorghr(N, iLO.`val`, iHI.`val`, y.data, scala.math.max(1, N), tau, workcnt, -1, info)
      lapack.dorghr(N, iLO.`val`, iHI.`val`, y.data, scala.math.max(1, N), tau, Array.ofDim[Double](workcnt(0).toInt), workcnt(0).toInt, info)

      //Error check
      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()

      (y, iLO.`val`, iHI.`val`, tau)
    }
  }

}