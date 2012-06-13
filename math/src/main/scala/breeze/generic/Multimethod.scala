package breeze.generic

import collection.mutable.HashMap

/**
 *
 * @author dlwh
 */

trait Multimethod[Method[AA, RR] <: MethodImpl[AA, RR], A <: AnyRef, R] extends MethodImpl[A, R] {
  this: Method[A, R] =>
  protected def bindingMissing(a: A): R = throw new UnsupportedOperationException("Types not found!")

  def apply(a: A): R = {
    ops.get(a.getClass) match {
      case None => bindingMissing(a)
      case Some(op) => op.asInstanceOf[Method[A, R]].apply(a)
    }
  }


  def register(a: Class[_ <: A], op: Method[_ <: A, _ <: R]) {
    ops(a) = op
  }

  def register[AA <: A](op: Method[AA, _ <: R])(implicit manA: Manifest[AA]) {
    register(manA.erasure.asInstanceOf[Class[AA]], op)
  }

  private val ops = HashMap[Class[_], Method[_ <: A, _ <: R]]()
}


// Doesn't extend Function1 because of implicit silliness.
trait MethodImpl[A,+R] {
  def apply(a: A):R
}

trait Multimethod2[Method[AA,BB,RR]<:Function2[AA,BB,RR],A<:AnyRef,B<:AnyRef,R] extends ((A, B) => R) { this: Method[A, B, R] =>
  protected def bindingMissing(a: A):R = throw new UnsupportedOperationException("Types not found!")

  def apply(a: A, b: B): R = {
    ops.get(a.getClass -> b.getClass) match {
      case None => bindingMissing(a)
      case Some(op) => op.asInstanceOf[Method[A, B, R]].apply(a, b)
    }
  }


  def register(a: Class[_<:A], b: Class[_<:B], op: Method[_ <: A, _ <: B ,_ <: R]) {
    ops(a -> b) = op
  }

  def register[AA<:A, BB<:B](op: Method[AA, BB, _ <: R])(implicit manA: Manifest[AA], manB: Manifest[BB]) {
    register(manA.erasure.asInstanceOf[Class[AA]], manB.erasure.asInstanceOf[Class[BB]], op)
  }

  private val ops = HashMap[(Class[_],Class[_]), Method[_ <: A, _ <: B, _ <: R]]()
}


trait Multiproc2[Method[AA,BB]<:(AA, BB) => Unit,A<:AnyRef,B] extends ((A, B) => Unit) { this: Method[A, B] =>
  protected def bindingMissing(a: A):Unit = throw new UnsupportedOperationException("Types not found!")

  def apply(a: A, b: B):Unit  = {
    ops.get(a.getClass -> b.asInstanceOf[AnyRef].getClass) match {
      case None => bindingMissing(a)
      case Some(op) => op.asInstanceOf[Method[A, B]].apply(a, b)
    }
  }


  def register(a: Class[_<:A], b: Class[_<:B], op: Method[_ <: A, _ <: B]) {
    ops(a -> b) = op
  }

  def register[AA<:A, BB<:B](op: Method[AA, BB])(implicit manA: Manifest[AA], manB: Manifest[BB]) {
    register(manA.erasure.asInstanceOf[Class[AA]], manB.erasure.asInstanceOf[Class[BB]], op)
  }

  private val ops = HashMap[(Class[_],Class[_]), Method[_ <: A, _ <: B]]()
}
