package scalanlp.util;

/*
* More scala like ThreadLocal storage.
*
* @author(dlwh)
*/
abstract class ThreadLocal[T] extends java.lang.ThreadLocal[T] with Function0[T] {
 protected override def initialValue() = default(); 
 // more scala like:
 protected def default(): T;

 def apply() = get();

 def value = get();
 def value_=(v : T) = set(v);
}
