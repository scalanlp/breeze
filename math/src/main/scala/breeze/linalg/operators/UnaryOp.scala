package breeze.linalg.operators

import collection.mutable.HashMap
import breeze.util.{MethodImpl, Multimethod}

/**
 *
 * @author dlwh
 */
trait UnaryOp[A, Op <: OpType, +R] extends MethodImpl[A, R] {
  def apply(a: A): R
}

object UnaryOp {
  type Bind[Op <:OpType] = { type Sig[A, R] = UnaryOp[A, Op, R]}
}

/**
 *
 * @author dlwh
 */

trait UnaryRegistry[A <: AnyRef, Op <: OpType, R] extends UnaryOp[A, Op, R] with Multimethod[UnaryOp.Bind[Op]#Sig, A, R] {
}

