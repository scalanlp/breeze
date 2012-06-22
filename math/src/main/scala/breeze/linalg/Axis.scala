package breeze.linalg

/**
 *
 * @author dlwh
 */

sealed trait Axis
object Axis {
  type Value = Axis
  case object _0 extends Axis
  case object _1 extends Axis
}


