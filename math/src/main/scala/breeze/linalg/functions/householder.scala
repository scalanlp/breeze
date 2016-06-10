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
import breeze.math._
import scala.annotation.tailrec
import breeze.generic.UFunc
import DenseMatrix.canMapValues

object householder extends UFunc {

  implicit object DMC_DVC_IMPL_H extends Impl2[DenseMatrix[Complex], DenseVector[Complex], Householder] {
    def apply(M: DenseMatrix[Complex], tau: DenseVector[Complex]): Householder = {
      new Householder(M, tau)
    }
  }

  implicit object DMC_IMPL_H extends Impl[DenseMatrix[Complex], Householder] {
    def apply(M: DenseMatrix[Complex]): Householder = {
      new Householder(M)
    }
  }

  class Householder(val matrixH: DenseMatrix[Complex], val coeffs: DenseVector[Complex]) {

    def this(matrixH: DenseMatrix[Complex]) = this(matrixH, DenseVector.zeros[Complex](matrixH.cols - 1))

    private val size = matrixH.cols - 1
    private val beta = Array.ofDim[Double](size)
    private val essential = Array.ofDim[DenseVector[Complex]](size)

    //returns Householder object (Householder matrix and householder coeffs)



    def generateFullHouseholder() =
    {
      for (icnt <- 0 to matrixH.rows - 2)
        applyHouseholder(icnt).applyHouseholderRight(icnt).applyHouseholderBottom(icnt)
this
    }

    def generateFullHouseholder(cnt:Int) =
    {
      for (icnt <- 0 to cnt)
        applyHouseholder(cnt).applyHouseholderRight(cnt).applyHouseholderBottom(cnt)
this
    }

    /*
   *  4 x 4 example
   *    x    x    x    x
   *    x    [ Row0 ]
   *    [e]    [  bott   ]
   *    [e]    [   bott  ]
   */
    def applyHouseholder(cnt: Int) = {
//compute A = H A H'
      essential(cnt) = matrixH((cnt + 2) to matrixH.rows - 1, cnt)

      val eNorm = if (essential(cnt).length == 0) 0.0 else sum(essential(cnt).map(x => scala.math.pow(x.real, 2) + scala.math.pow(x.imag, 2))) // Does Complex component need squaring?
      val c0 = matrixH(cnt + 1, cnt);
      (eNorm, c0.imag) match {
        case (0, 0) =>
          beta(cnt) = c0.real
          coeffs(cnt) = Complex(0, 0)
        case _ =>
          val c0norm = scala.math.pow(c0.real, 2) + scala.math.pow(c0.imag, 2)
          beta(cnt) = if (c0.real >= 0) -Math.sqrt(c0norm + eNorm) else Math.sqrt(c0norm + eNorm)
          coeffs(cnt) = ((beta(cnt) - c0) / beta(cnt))
          essential(cnt) = (essential(cnt) / (c0 - beta(cnt)))
      }

      matrixH((cnt + 1), cnt) = Complex(beta(cnt), 0)
      matrixH((cnt + 2) to matrixH.rows - 1, cnt) := essential(cnt)

      val matH2 = matrixH.mapValues(_.real)
      this
    }
    /*
     *  4 x 4 example
     *    x    [c0]   [Right]
     *    x    [c0]   [Right]
     *    [e]    [c0]   [Right]
     *    [e]    [c0]    [Right]
     */
    def applyHouseholderRight(cnt: Int) = {
  // A = H A
      if (matrixH.cols == 1) {
        matrixH *= 1 - coeffs(cnt)
        this
      } else {
        var c0 = matrixH(::, (cnt + 1) to (cnt + 1))
        var right = matrixH(::, (cnt + 2) to matrixH.cols - 1)
        val tmp = (right * (essential(cnt).toDenseMatrix.map(x => Complex(x.real, -x.imag))).t) + c0
        c0 -= tmp * coeffs(cnt)
        right -= tmp * coeffs(cnt) * essential(cnt).toDenseMatrix

        this
      }
    }
    /*
     *  4 x 4 example
     *    x    x    x    x
     *    x    [     r0    ]
     *    [e]     [Bottom]
     *    [e]    [Bottom]
     */


    def applyHouseholderBottom(cnt: Int) = {
    // A = A H'
      if (matrixH.cols == 1) {
        matrixH *= 1 - coeffs(cnt)
        this
      } else {
        var r0 = matrixH((cnt + 1), (cnt + 1) to matrixH.cols - 1).t
        var bottom = matrixH((cnt + 2) to matrixH.rows - 1, (cnt + 1) to matrixH.cols - 1)
        val tmp = (bottom.t * essential(cnt).map(x => Complex(x.real, -x.imag))) + r0
        r0 -= (tmp.toDenseMatrix * coeffs(cnt)).toDenseVector
        bottom -= (essential(cnt).toDenseMatrix.t * tmp.toDenseMatrix) * coeffs(cnt)

        this
      }
    }
  }
  /*
   *  4 x 4 example
   *    x    x    x    x
   *    c0  x    x    x
   *    e    x    x    x
   *    e    x    x    x
   */
  //def apply(m_matrix: DenseMatrix[Complex]): householder = new householder(m_matrix)
//does householder require Complex coefficients? .....
  def householderTransformation(hMatrix: DenseMatrix[Double], coeffs: DenseVector[Double], order: Int): DenseMatrix[Double] = {
    //householderTransformation shifted 1 with size -1
    /*  4 x 4 example of the form
       *  1    0    0     0   ^  ------- order (wrapper)
       *   0    1    0    0   v
       *   0    0     x    x
       *   0    0     x    x
       */
    if ((hMatrix.rows - order) != coeffs.length)
      throw new MatrixNotSquareException // change to correct exception

    val matHS = hMatrix(order to (hMatrix.cols - 1), 0 to (hMatrix.rows - order - 1))

    val I = DenseMatrix.eye[Double](matHS.cols)
    val hhMv = (upperTriangular(matHS.t) *:* -(I - 1.0)) +:+ I
    var cnt = 0

    val sum2 = (new ((Int, Int, DenseMatrix[Double]) => DenseMatrix[Double]) {
      @tailrec def apply(from: Int, to: Int, s: DenseMatrix[Double]): DenseMatrix[Double] = {
        if (from == to) return s;
        apply(from + 1, to, s * -(coeffs(from) * hhMv(from, ::).t * hhMv(from, ::).t.toDenseMatrix - I))
      }
    })(0, matHS.cols - 1, I)

    if (order != 0) {
      val wrapper = DenseMatrix.zeros[Double](hMatrix.cols, hMatrix.cols)
      wrapper(order to wrapper.cols - 1, order to wrapper.cols - 1) := sum2
      for (cnt <- 0 to order - 1) {
        wrapper(cnt, cnt) = 1.0;
      }
      wrapper
    } else
      sum2
  }
}
