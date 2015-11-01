package breeze.linalg

import breeze.generic.UFunc
import spire.implicits.cforRange


/**
 * Computes the log of the determinant of the given real matrix. The  value returned
 * is (sign of determinant, log of determinant). This method can be more accurate than just using [[breeze.linalg.det]],
 * if det is very small.
 */
object logdet extends UFunc {
  implicit def canDetUsingLU[T](implicit luImpl: LU.Impl[T, (DenseMatrix[Double], Array[Int])]):Impl[T, (Double, Double)] = {
    new Impl[T, (Double, Double)] {
      def apply(X: T): (Double, Double) = {

        // For triangular N-by-N matrices X, the determinant of X equals the product
        // of the diagonal elements X(i,i) where 0 <= i < N.
        // Since det(AB) = det(A) * det(B), the LU factorization is well-suited for
        // the computation of the determinant of general N-by-N matrices.
        val (m:DenseMatrix[Double], ipiv:Array[Int]) = LU(X)

        // Count the number of exchanged rows.  ipiv contains an array of swapped indices,
        //  but it also contains indices that weren't swapped.  To count the swapped
        //  indices, we have to compare them against their position within the array.  A
        //  final complication is that the array indices are 1-based, due to the LU call
        //  into LAPACK.
        val numExchangedRows = ipiv.map(_ - 1).zipWithIndex.count { piv => piv._1 != piv._2 }

        var sign = if (numExchangedRows % 2 == 1) -1.0 else 1.0

        var acc = 0.0
        cforRange(0 until m.rows){ i =>
          val mii = m(i, i)
          if(mii == 0.0) return (0.0, Double.NegativeInfinity)
          acc += math.log(math.abs(mii))
          sign *= math.signum(mii)
        }

        (sign, acc)
      }

    }
  }
}
