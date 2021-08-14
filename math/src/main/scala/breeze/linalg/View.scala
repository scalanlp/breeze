package breeze.linalg

/**
 * Trait used for methods that can return a view or a copy.
 * @author dlwh
 */
sealed trait View

object View {
  implicit def viewPreferenceFromBoolean(b: Boolean): View = if (b) Require else Copy

  case object Require extends View
  case object Copy extends View
  case object Prefer extends View

}
