package breeze.generic

/**
 * Marker for being able to map the keys and values in a value collection
 * to new values.
 *
 * @author dramage
 * @author dlwh
 */
trait CanMapValues[-From, +A, -B, +To] {
  /**Maps all key-value pairs from the given collection. */
  def map(from: From, fn: (A => B)): To

  /**Maps all active key-value pairs from the given collection. */
  def mapActive(from: From, fn: (A => B)): To
}

object CanMapValues {
  implicit def canMapSelf[V, V2]: CanMapValues[V, V, V2, V2] = {
    new CanMapValues[V, V, V2, V2] {
      def map(from: V, fn: (V) => V2) = fn(from)
      def mapActive(from: V, fn: (V) => V2) = fn(from)
    }
  }
}