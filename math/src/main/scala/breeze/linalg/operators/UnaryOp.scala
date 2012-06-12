package breeze.linalg.operators

import collection.mutable.HashMap

/**
 *
 * @author dlwh
 */
trait UnaryOp[-A, Op <: OpType, +R] {
  def apply(a: A): R
}

/**
 *
 * @author dlwh
 */

trait UnaryRegistry[A <: AnyRef, Op <: OpType, R] extends UnaryOp[A, Op, R] {

  private val ops = HashMap[Class[_], UnaryOp[_ <: A, Op, _ <: R]]()

  protected var parent: UnaryRegistry[_ >: A, Op, _ >: R] = null

  def registerUnary(a: Class[_], b: Class[_], op: UnaryOp[_ <: A, Op, _ <: R]) {
    if (parent ne null) parent.registerUnary(a, b, op)
    ops(a) = op
  }

  def apply(a: A): R = {
    ops.get(a.getClass) match {
      case None => doOp(a)
      case Some(op) => op.asInstanceOf[UnaryOp[A, Op, R]].apply(a)
    }
  }

  def doOp(a: A): R
}
