package minla.linalg.support

/**
 * Marker for being able to map the keys and values in a value collection
 * to new values.
 *
 * @author dramage
 * @author dlwh
 */
trait CanMapKeyValuePairs[-From, +K, +A, -B, +To] {
  /** Maps all key-value pairs from the given collection. */
  def map(from : From, fn : ((K,A) => B)) : To

  /** Maps all active key-value pairs from the given collection. */
  def mapActive(from : From, fn : ((K,A) => B)) : To
}
