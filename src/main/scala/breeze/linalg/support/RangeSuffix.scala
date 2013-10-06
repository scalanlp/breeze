package breeze.linalg.support

/**
 * A range suffix is just a marker for the beginning of a range.
 *
 * @author dlwh
 **/
class RangeSuffix(val start: Int) extends AnyVal {
  def step = 1
}
