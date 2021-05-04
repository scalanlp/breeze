package breeze.gymnastics

/** Implicit witness that means you can find an A and a B. Mostly for using with [[scala.util.NotGiven]]  */
sealed trait &:&[+A, +B] extends Serializable

object &:& {
  private val inst: Nothing &:& Nothing = new &:&[Nothing, Nothing]{}
  implicit def a_and_b[A, B](implicit a: A, b: B): A &:& B = inst
}