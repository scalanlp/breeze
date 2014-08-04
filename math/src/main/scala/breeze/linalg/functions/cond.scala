package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.svd.{SVD, DenseSVD}


/**
 * Computes the condition number of the given real matrix.
 */
object cond extends UFunc {
  implicit def canDetUsingSVD[T](implicit svdImpl: svd.Impl[T, DenseSVD]):Impl[T, Double] = {
    new Impl[T, Double] {
      def apply(X: T): Double = {
        val SVD(_, vecs, _) = svd(X)
        vecs(0)/vecs(vecs.length - 1)
      }
    }
  }
}
