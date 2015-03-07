package breeze.linalg


/**
 * Marker trait for exceptions thrown from the [[breeze.linalg]] package.
 */
trait LinearAlgebraException extends RuntimeException

/**
 * Exception thrown if a routine has not converged.
 */
class NotConvergedException(val reason: NotConvergedException.Reason, msg: String = "")
  extends RuntimeException(msg) with LinearAlgebraException

object NotConvergedException {
  trait Reason
  object Iterations extends Reason
  object Divergence extends Reason
  object Breakdown extends Reason
}

class MatrixNotSymmetricException extends IllegalArgumentException("Matrix is not symmetric") with LinearAlgebraException

class MatrixNotSquareException extends IllegalArgumentException("Matrix is not square") with LinearAlgebraException

class MatrixEmptyException extends IllegalArgumentException("Matrix is empty") with LinearAlgebraException

/**
 * Thrown when trying to solve using a singular matrix.
 *
 * @author dramage, dlwh
 */
class MatrixSingularException(msg : String="") extends RuntimeException(msg)  with LinearAlgebraException

class LapackException(msg: String="") extends RuntimeException(msg) with LinearAlgebraException