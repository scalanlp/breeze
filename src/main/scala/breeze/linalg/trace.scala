package breeze.linalg

import breeze.generic.UFunc


/**
 * Computes the determinant of the given real matrix.
 */
object trace extends UFunc {
  implicit def canTraceUsingDiagAndSum[T, U, V](implicit diagImpl: diag.Impl[T, U], sumImpl: sum.Impl[U, V]):Impl[T, V] = {
    new Impl[T, V] {
      def apply(X: T): V = {
        sumImpl(diagImpl(X))
      }
    }
  }
}

