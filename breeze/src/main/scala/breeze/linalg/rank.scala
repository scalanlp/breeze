package breeze.linalg

import breeze.generic.UFunc
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor

/**
 * Computes the rank of a matrix.
 *
 * The rank of the matrix is computed using the SVD method.  The singular values of the SVD
 * which are greater than a specified tolerance are counted.
 *
 * @param m matrix for which to compute the rank
 * @param tol optional tolerance for singular values.  If not supplied, the default
 *   tolerance is: max(m.cols, m.rows) * eps * sigma_max, where
 *   eps is the machine epsilon and sigma_max is the largest singular value of m.
 * @return the rank of the matrix (number of singular values)
 */
object rank extends UFunc {
  implicit def implRankFromSVD[M, S](implicit canSVD: svd.Impl[M, (_, S, _)],
                                     maxS: max.Impl[S, Double],
                                     travS: CanTraverseValues[S, Double]):Impl[M, Int] = {
    new Impl[M, Int] {
      def apply(m: M): Int = {
        val (u, s, vt) = svd(m)
        // we called LAPACK for the SVD method, so this is the LAPACK definition of eps.
        val eps: Double = 2.0 * lapack.dlamch("e")
        val tol = eps * max(s)
        var n = 0
        travS.traverse(s, new ValuesVisitor[Double] {
          def visit(a: Double): Unit = if(a > tol) n += 1

          def zeros(numZero: Int, zeroValue: Double): Unit = ()
        })

        n
      }
    }

  }

  implicit def implRankTol[M, S](implicit canSVD: svd.Impl[M, (_, S, _)],
                                     maxS: max.Impl[S, Double],
                                     travS: CanTraverseValues[S, Double]):Impl2[M, Double, Int] = {
    new Impl2[M, Double, Int] {
      def apply(m: M, tol: Double): Int = {
        val (u, s, vt) = svd(m)
        // we called LAPACK for the SVD method, so this is the LAPACK definition of eps.
        var n = 0
        travS.traverse(s, new ValuesVisitor[Double] {
          def visit(a: Double): Unit = if(a > tol) n += 1

          def zeros(numZero: Int, zeroValue: Double): Unit = ()
        })

        n
      }
    }

  }



}
