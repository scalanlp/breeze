package scalanlp.util;

class Lazy[+T](x: =>T) extends Function0[T] {
  lazy val result: T = x;
  def apply() = result;

  def map[U](f: T=>U) = Lazy.delay { f(result) }

  def flatMap[U](f: T=>Lazy[U]) = Lazy.delay { f(result).result}

  def foreach(f: T=>Unit) = f(result);
}

object Lazy {
  object Implicits {
    implicit def fromLazy[T](l: Lazy[T]) = l.result;
  }

  def delay[T](x: =>T) = new Lazy[T](x);

  def apply[T](x: =>T) = delay(x);

}
