package minla.linalg.operators

import collection.mutable.HashMap

/**
 *
 * @author dlwh
 */
trait BinaryOp[A, B, +Op<:OpType, R] {
  def apply(a: A, b: B): R
}

/**
 *
 * @author dlwh
 */

trait BinaryRegistry[A<:AnyRef, B<:AnyRef, Op<:OpType, R] extends BinaryOp[A, B, Op, R] {

  private val ops = HashMap[(Class[_], Class[_]), BinaryOp[_ <: A, _ <: B, Op, _ <: R]]()

  def registerBinary(a: Class[_<:A], b: Class[_<:B], op: BinaryOp[_ <: A, _ <: B, Op, _ <: R]) {
    ops(a -> b) =  op
  }

  def registerBinary[AA<:A, BB<: B](op: BinaryOp[_ <: A, _ <: B, Op, _ <: R])(implicit manA: Manifest[A], manB: Manifest[B]) {
    registerBinary(manA.erasure.asInstanceOf[Class[AA]], manB.erasure.asInstanceOf[Class[BB]], op)
  }

  def apply(a: A, b: B): R = {
    ops.get(a.getClass -> b.getClass) match {
      case None => doOp(a, b)
      case Some(op) => op.asInstanceOf[BinaryOp[A, B, Op, R]].apply(a,b)
    }
  }

  def doOp(a: A, b: B): R
}
