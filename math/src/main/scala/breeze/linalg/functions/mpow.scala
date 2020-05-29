package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.eig.Eig

/**
 * Raises m to the exp'th power. Relies on eigenvalue decomposition when m's
 * eigenvalues are real, if not it relies on exponentiation by squaring.
 */
object mpow extends UFunc {

  private def powBySquaring(m: DenseMatrix[Double], exp: Int): DenseMatrix[Double] = {
    // Assuming that m is square because it has already been verified
    if (exp == 0) {
      DenseMatrix.eye[Double](m.rows)
    } else if (exp < 0) {
      powBySquaring(inv(m), -exp)
    } else {
      var res_odds = DenseMatrix.eye[Double](m.rows)
      var res = m
      var n = exp
      while (n > 1) {
        if (n % 2 == 0) {
          res = res * res
          n = n / 2
        } else {
          res_odds = res * res_odds
          res = res * res
          n = (n - 1) / 2
        }
      }
      res * res_odds
    }
  }

  implicit object implDM_Double_Double extends Impl2[DenseMatrix[Double], Double, DenseMatrix[Double]] {
    def apply(m: DenseMatrix[Double], exp: Double): DenseMatrix[Double] = {
      requireSquareMatrix(m)
      val Eig(real, imag, evectors) = eig(m)
      if (norm(imag, 1.0) == 0.0) {
        val exped = new DenseVector(real.data.map(scala.math.pow(_, exp)))

        (evectors.t \ (evectors * diag(exped)).t).t
      } else {
        require(exp % 1 == 0, "If m has complex eigenvalues exp need to be integer")
        powBySquaring(m, exp.toInt)
      }
    }
  }

  implicit object implDM_Double_Int extends Impl2[DenseMatrix[Double], Int, DenseMatrix[Double]] {
    def apply(m: DenseMatrix[Double], exp: Int): DenseMatrix[Double] = {
      implDM_Double_Double(m, exp.toDouble)

    }
  }

}
