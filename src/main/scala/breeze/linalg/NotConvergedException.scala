package breeze.linalg


/**
 * Exception thrown if a routine has not converged.
 */
class NotConvergedException(val reason: NotConvergedException.Reason, msg: String = "")
  extends RuntimeException(msg)

object NotConvergedException {
  trait Reason
  object Iterations extends Reason
  object Divergence extends Reason
  object Breakdown extends Reason
}

class MatrixNotSymmetricException extends IllegalArgumentException("Matrix is not symmetric")

class MatrixNotSquareException extends IllegalArgumentException("Matrix is not square")

class MatrixEmptyException extends IllegalArgumentException("Matrix is empty")
