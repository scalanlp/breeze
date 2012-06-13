package breeze.linalg.operators

import collection.mutable.HashMap
import breeze.generic.Multimethod2

/**
 *
 * @author dlwh
 */
trait BinaryOp[A, B, +Op<:OpType, R] extends ((A, B) => R) {
  def apply(a: A, b: B): R
}

object BinaryOp {
  type Bind[Op <:OpType] = { type Sig[A, B, R] = BinaryOp[A, B, Op, R]}
}

/**
 *
 * @author dlwh
 */

trait BinaryRegistry[A<:AnyRef, B<:AnyRef, Op<:OpType, R] extends BinaryOp[A, B, Op, R] with Multimethod2[BinaryOp.Bind[Op]#Sig, A, B, R]{

}
