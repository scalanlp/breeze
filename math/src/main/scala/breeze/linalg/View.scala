package breeze.linalg

/**
 * Trait used for methods that can return a view or a copy.
 * @author dlwh
 */
sealed trait View

//ToDo 3: Could View be made part of the Opts hierarchy for options?
object View {
  implicit def viewPreferenceFromBoolean(b: Boolean) = if (b) Require else Copy

  case object Require extends View
  case object Copy extends View
  case object Prefer extends View

}
