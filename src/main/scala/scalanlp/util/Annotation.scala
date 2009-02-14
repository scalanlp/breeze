package scalanlp.util

import scala.reflect.Manifest;
import scala.collection.immutable._;

class Annotation[+T] (private val map: Map[Class[_],Any]) {
  def get[U>:T](implicit m: Manifest[U]) = map.get(m.erasure).asInstanceOf[Option[U]];
  def apply[U>:T]()(implicit m: Manifest[U]) = get[U].get;
  def +[U](x: U)(implicit m: Manifest[U]) = new Annotation[T with U](map + (m.erasure->x))
  override def toString = map.mkString("Annotation(",",",")");
}

object Annotation {
  def apply() = new Annotation[Any](Map.empty);
  def apply[T](x:T)(implicit m: Manifest[T]) = new Annotation[T](Map.empty + (m.erasure->x))
}