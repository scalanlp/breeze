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

import breeze.numerics._
import breeze.math._
import breeze.linalg.Helper._
import scala.annotation.tailrec
import breeze.generic.UFunc
import DenseMatrix.canMapValues
import breeze.linalg.revertSchur._

/*
 * Based on:
 *A SCHUR–PADÉ ALGORITHM FOR
 * FRACTIONAL POWERS OF A MATRIX*
 * NICHOLAS J. HIGHAM† AND LIJING LIN†

 * The algorithm starts with a Schur decomposition, takes kk square roots of the triangular factor TT,  * evaluates
 *  an [m/mm/m] Padé approximant of (1−x)p(1-x)p at I−T1/2kI-T1/2k, and squares the result
 * kk times. The parameters kk and mm are chosen to minimize the cost subject to achieving double precision
 * accuracy in the evaluation of the Padé approximant, making use of a result that bounds the error in the
 *  matrix Padé approximant by the error in the scalar Padé approximant with argument the norm of the matrix.
 * The Padé approximant is evaluated from the continued fraction representation in bottom-up fashion,
 *  which is shown to be numerically stable. In the squaring phase the diagonal and first superdiagonal are
 *  computed from explicit formulae for Tp/2jTp/2j, yielding increased accuracy. Since the basic algorithm is
 * designed for p∈(−1,1)p∈(-1,1), a criterion for reducing an arbitrary real pp to this range is developed,
 * making use of bounds for the condition number of the ApAp problem. How best to compute AkAk for a
 * negative integer kk is also investigated. In numerical experiments the new algorithm is found to be
 * superior in accuracy and stability to several alternatives, including the use of an eigendecomposition
 * and approaches based on the formula Ap=exp(plog(A))Ap=exp(plog(A)).

 Read More: http://epubs.siam.org/doi/abs/10.1137/10081232X?journalCode=sjmael
 */

object matrixPow extends UFunc {

  implicit object DMD_IMPL_D_DMC extends Impl2[DenseMatrix[Double], Double, DenseMatrix[Complex]] {
    def apply(M: DenseMatrix[Double], exp: Double): DenseMatrix[Complex] = {
      fractD(exp, M)
    }
  }
  implicit object DMI_IMPL_D_DMC extends Impl2[DenseMatrix[Int], Double, DenseMatrix[Complex]] {
    def apply(M: DenseMatrix[Int], exp: Double): DenseMatrix[Complex] = {
      fractI(exp, M)
    }
  }
  implicit object DMC_IMPL_D_DMC extends Impl2[DenseMatrix[Complex], Double, DenseMatrix[Complex]] {
    def apply(M: DenseMatrix[Complex], exp: Double): DenseMatrix[Complex] = {
      fractC(exp, M)
    }
  }

  private def padeDegree(normIminusT: Double): Int = {
    return List(0.01884160592658218, 0.06038881904059573, 0.1239917516308172, 0.1999045567181744, 0.2789358995219730).indexWhere(normIminusT <= _.self) match {
      case r if r > 0 => r + 3
      case _ => 7
    }
  }
  @deprecated("Finding way to  implement LU for complex numbers", "0.1")
  private def padePower(IminusT: DenseMatrix[Complex], m_p: Double, degree: Int) = {

    debugPrint(IminusT, "Before padePower", 4)
    debugPrint(degree, "_degree in pade Solver", 4)
    val utl = degree << 1

    val eye = DenseMatrix.eye[Complex](IminusT.rows)
    var res = IminusT.map(_ * (m_p - degree.toDouble) / ((utl - 1) << 1))

    debugPrint(res, "start res  in pade Solver", 4)
    val i = utl - 1
    for (i <- i to 1 by -1) {
      debugPrint(i, "i in pade Solver", 4)
      val T1 = if (i == 1) -m_p else if ((1 & i) == 1) (-m_p - (i >> 1)) / (i << 1) else (m_p - (i >> 1)) / ((i - 1) << 1)
      val T = IminusT * Complex(T1, 0)
      debugPrint(T, "T in pade Solver", 4)
      res = ((eye + res).mapValues(_.real) \ T.mapValues(_.real)).mapValues(Complex(_, 0.0)) // BIG PROBLEMMMO
      debugPrint(res, "M in pade Solver", 4)
      res = res + eye
       }
   debugPrint(res, "After padePower", 4)
   res
    }

  private def sqrtTriangular(m_A: DenseMatrix[Complex]) = {
    val result = DenseMatrix.zeros[Complex](m_A.cols, m_A.rows)
    var i = 0
    for (i <- 0 to m_A.cols - 1)
      result(i, i) = breeze.numerics.pow(m_A(i, i), 0.5)
    var j = 1
    for (j <- 1 until m_A.cols) {
      for (i <- (j - 1) to 0 by -1) {
        val tmp = result(i, (i + 1) to (j - i)) * result((i + 1) to (j - i), j)
        result(i, j) = (m_A(i, j) - tmp) / (result(i, i) + result(j, j))
      }
    }
    result
  }

  private val maxNormForPade: Double = 2.789358995219730e-1

  private def getIMinusT(T: DenseMatrix[Complex], numSquareRoots: Int = 0, deg1: Double = 10.0, deg2: Double = 0.0): (DenseMatrix[Complex], Int, Int) = {

    val IminusT = DenseMatrix.eye[Complex](T.rows) - T
    val cols = IminusT(::, *).map(_.foldLeft(Complex(0.0, 0.0))((j, k) => Complex((pow(pow(abs(j.real), 2) + pow(abs(j.imag), 2), 0.5) + pow(pow(abs(k.real), 2) + pow(abs(k.imag), 2), 0.5)), 0.0))).t.mapValues(_.real)
    val normIminusT = max(cols)

    debugPrint(IminusT, "IminusT", 5)
    debugPrint(normIminusT, "normIminusT", 5)
    debugPrint(numSquareRoots, "numSquareRoots", 5)
    if (normIminusT < maxNormForPade) {
      val rdeg1 = padeDegree(normIminusT)
      val rdeg2 = padeDegree(normIminusT / 2)

      debugPrint(rdeg1, "rdeg1", 5)
      debugPrint(rdeg2, "rdeg2", 5)

      if (rdeg1 - rdeg2 <= 1.0)
        return (IminusT, numSquareRoots, rdeg1)
      else
        getIMinusT(upperTriangular(sqrtTriangular(T)), numSquareRoots, rdeg1, rdeg2)
    }
    getIMinusT(upperTriangular(sqrtTriangular(T)), numSquareRoots + 1, deg1, deg2)
  }

  private def computeSuperDiag(curr: Complex, prev: Complex, p: Double): Complex = {

    val logCurr: Complex = log(curr)
    val logPrev: Complex = log(prev)
    val unwindingNumber = ceil(((logCurr - logPrev).imag - M_PI) / (2 * M_PI))
    val w = atan2h(curr - prev, curr + prev) + Complex(0.0, M_PI * unwindingNumber)
    (2.0 * exp(0.5 * p * (logCurr + logPrev)) * sinh2(p * w) / (curr - prev))
  }

  private def compute2x2(r: DenseMatrix[Complex], m_A: DenseMatrix[Complex], p: Double) = {

    val res = r
    res(0, 0) = pow(m_A(0, 0), p)
    var i = 1
    for (i <- 1 until m_A.cols) {
      res(i, i) = pow(m_A(i, i), p)

      if (m_A(i - 1, i - 1) == m_A(i, i)) {
        res(i - 1, i) = p * pow(m_A(i, i), p - 1)
      } else if (2 * abs(m_A(i - 1, i - 1)) < abs(m_A(i, i)) || 2 * abs(m_A(i, i)) < abs(m_A(i - 1, i - 1))) {
        res(i - 1, i) = (res(i, i) - res(i - 1, i - 1)) / (m_A(i, i) - m_A(i - 1, i - 1))
      } else {
        res(i - 1, i) = computeSuperDiag(m_A(i, i), m_A(i - 1, i - 1), p)
      }
      res(i - 1, i) *= m_A(i - 1, i)
    }
    res
  }

  private def computeIntPowerD(exp: Double, value: DenseMatrix[Double]): DenseMatrix[Double] = {
    if (exp == 1) value
    else if (exp % 2 == 1) value * (computeIntPowerD(exp - 1, value))
    else {
      val half = computeIntPowerD(exp / 2, value)
      half * half
  }
    }

  private def computeIntPowerC(exp: Double, value: DenseMatrix[Complex]): DenseMatrix[Complex] = {
    if (exp == 1) value
    else if (exp % 2 == 1) value * (computeIntPowerC(exp - 1, value))
    else {
      val half = computeIntPowerC(exp / 2, value)
      half * half
  }
    }

  @tailrec
  private def computeFracPower(value: DenseMatrix[Complex], sT: DenseMatrix[Complex], frac_power: Double, noOfSqRts: Int): DenseMatrix[Complex] = {
    if (noOfSqRts == 1)
      return value * upperTriangular(compute2x2(value, sT, frac_power * scala.math.pow(2, -noOfSqRts)))
    else
      computeFracPower(value * upperTriangular(compute2x2(value, sT, frac_power * scala.math.pow(2, -noOfSqRts))), sT, frac_power, noOfSqRts - 1)
  }

  private def cFracPart(M: DenseMatrix[Complex], pow: Double): (Option[DenseMatrix[Complex]], DenseMatrix[Complex]) = {
    val (sT, sQ, tau, house) = schur(M)
    val fpow = pow - floor(pow)
    if (abs(fpow) > 0) {
      val (iminusT, noOfSqRts, degree) = getIMinusT(upperTriangular(sT))
      val pT = computeFracPower(padePower(iminusT, fpow, degree), sT, fpow, noOfSqRts)
      (Some(pT), sQ)
    } else {
      (None, sQ)
  }
    }

  private def dFracPart(MD: DenseMatrix[Double], pow: Double): (Option[DenseMatrix[Complex]], DenseMatrix[Complex]) = {
    val (sT, sQ, tau, house) = schur(MD)
    val M = MD.mapValues(Complex(_, 0.0))
    val fpow = pow - floor(pow)

    if (abs(fpow) > 0) {
      val (iminusT, noOfSqRts, degree) = getIMinusT(upperTriangular(sT))
      val pT = computeFracPower(padePower(iminusT, fpow, degree), sT, fpow, noOfSqRts)
      (Some(pT), sQ)
    } else
      (None, sQ)
  }

  private def fractC(pow: Double, M: DenseMatrix[Complex]): DenseMatrix[Complex] = {
    val ipower = abs(floor(pow))
    val intPow = if (pow < 0.0)
      throw new IllegalArgumentException("Cannot currently invert complex matrices.")

    return cFracPart(M, pow) match {
      case (None, _) => computeIntPowerC(ipower, M)
      case (pT, sQ) => if (ipower > 0) revertSchur(pT.get, sQ) * computeIntPowerC(ipower, M) else revertSchur(pT.get, sQ)
  }
    }

  private def fractI(pow: Double, M: DenseMatrix[Int]): DenseMatrix[Complex] = fractD(pow, M.mapValues(_.toDouble))
  private def fractD(pow: Double, M: DenseMatrix[Double]): DenseMatrix[Complex] = {
    val ipower = abs(floor(pow))
    val intPow = if (pow < 0.0) inv(M) else M
    return dFracPart(M, pow) match {
      case (None, _) => computeIntPowerD(ipower, intPow).mapValues(Complex(_, 0.0))
      case (pT, sQ) => if (ipower > 0) revertSchur(pT.get, sQ) * computeIntPowerD(ipower, intPow).mapValues(Complex(_, 0.0)) else revertSchur(pT.get, sQ)
  }
    }
}
