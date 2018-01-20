package breeze.linalg.support

/**
 * Marker for being able to do a foreach on the values of a collection
 *
 * @author dlwh
 */
trait CanForeachValues[From, +A] {

  /**Maps all key-value pairs from the given collection. */
  def foreach[U](from: From, fn: (A => U)): Unit

}
