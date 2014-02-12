package breeze.linalg

import breeze.generic.UFunc


/**
 * Computes the condition number of the given real matrix.
 */
object cond extends UFunc {
  implicit def canDetUsingSVD[T](implicit luImpl: svd.Impl[T,  (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double])]):Impl[T, Double] = {
    new Impl[T, Double] {
      def apply(X: T): Double = {
        val (_, vecs, _) = svd(X)
        vecs(0)/vecs(vecs.length - 1)
      }
    }
  }
}
