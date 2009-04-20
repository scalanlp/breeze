package scalanlp.util;

class Lazy[+T](x: =>T) extends Function0[T] {
  lazy val result: T = x;
  def apply() = result;
}

object Lazy {
  object Implicits {
    implicit def fromLazy[T](l: Lazy[T]) = l.result;
  }

  def delay[T](x: =>T) = new Lazy[T](x);

  def apply[T](x: =>T) = delay(x);

}
