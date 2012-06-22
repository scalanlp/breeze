package breeze.linalg.support

/**
 *
 * @author dlwh
 */
trait CanSlice[-From, -Slice, +To] {
 def apply(from: From, slice: Slice):To
}

/**
 *
 * @author dlwh
 */
trait CanSlice2[-From, -Slice1, -Slice2, +To] {
  def apply(from: From, slice: Slice1, slice2: Slice2):To
}
