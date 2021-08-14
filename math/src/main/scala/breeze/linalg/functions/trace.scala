package breeze.linalg

import breeze.generic.UFunc

/**
 * Computes the determinant of the given real matrix.
 */
object trace extends UFunc {
  implicit def impl_trace_using_diag_and_sum[T, U, V](
      implicit diagImpl: diag.Impl[T, U],
      sumImpl: sum.Impl[U, V]): Impl[T, V] = {
    new Impl[T, V] {
      def apply(X: T): V = {
        sumImpl(diagImpl(X))
      }
    }
  }
}
