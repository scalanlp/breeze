package scalanlp.util;

final case class Ref[T](private var x:T) {
  def unary_! = x;

  def :=(newX:T) {x = newX}
}
