package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.eig.Eig

/**
 * Raises m to the exp'th power via eigenvalue decomposition. Currently requires
 * that m's eigenvalues are real.
 */
object mpow extends UFunc {

  implicit object implDM_Double_Double extends Impl2[DenseMatrix[Double], Double, DenseMatrix[Double]] {
    def apply(m: DenseMatrix[Double], exp: Double): DenseMatrix[Double] = {
      requireSquareMatrix(m)
      val Eig(real, imag, evectors) = eig(m)
      require(norm(imag, 1.0) == 0.0, "We cannot handle complex eigenvalues yet.")
      val exped = new DenseVector(real.data.map(scala.math.pow(_, exp)))

      (evectors.t \ (evectors * diag(exped)).t).t
    }
  }

  implicit object implDM_Double_Int extends Impl2[DenseMatrix[Double], Int, DenseMatrix[Double]] {
    def apply(m: DenseMatrix[Double], exp: Int): DenseMatrix[Double] = {
      implDM_Double_Double(m, exp.toDouble)

    }
  }

}
