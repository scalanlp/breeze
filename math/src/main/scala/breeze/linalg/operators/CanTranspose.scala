package breeze.linalg.operators

/**
 * Transpose of a shaped value.
 *
 * @author dramage
 * @author dlwh
 */
trait CanTranspose[-From, +To] {
  def apply(from: From):To
}
