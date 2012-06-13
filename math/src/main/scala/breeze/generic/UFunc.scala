package breeze.generic

/**
 * "Universal" Functions that mimic numpy's. A universal function is defined
 * on anything that supports elementwise maps
 *
 * For example, exp is an UFunc: it just calls exp on all components of the passed in
 * object.
 * @author dlwh
 */
trait UFunc[@specialized -V, @specialized +V2] {
  def apply(v: V):V2
  def apply[T,U](t: T)(implicit cmv: CanMapValues[T, V, V2, U]):U = cmv.map(t, apply _)
  def applyActive[T,U](t: T)(implicit cmv: CanMapValues[T, V, V2, U]):U = cmv.mapActive(t, apply _)
}

object UFunc {
  def apply[V, V2](f: V=>V2):UFunc[V, V2] = new UFunc[V, V2] {
    def apply(v: V) = f(v)
  }
}
