package minla.linalg.operators
import collection.mutable.HashMap


trait BinaryUpdateOp[A, B, Op<:OpType] {
  protected var parent: BinaryUpdateRegistry[_ >: A, _ >: B, Op] = null
  def apply(a: A, b: B)
  protected def setParent(parent: BinaryUpdateRegistry[_ >: A, _ >: B, Op])(implicit manA: Manifest[A], manB: Manifest[B]) = {
    this.parent = parent
    parent.registerBinary(manA.erasure, manB.erasure, this)
  }
}


trait BinaryUpdateRegistry[A<:AnyRef, B, Op<:OpType] extends BinaryUpdateOp[A, B, Op] {
  private val ops = HashMap[(Class[_], Class[_]), BinaryUpdateOp[_ <: A, _ <: B, Op]]()

  def registerBinary(a: Class[_], b: Class[_], op: BinaryUpdateOp[_ <: A, _ <: B, Op]) {
    if (parent ne null) parent.registerBinary(a, b, op)
    ops(a -> b) = op
  }

  def apply(a: A, b: B) {
    ops.get(a.getClass -> b.asInstanceOf[AnyRef].getClass) match {
      case None => doOp(a, b)
      case Some(op) => op.asInstanceOf[BinaryUpdateOp[A, B, Op]].apply(a,b)
    }
  }

  def doOp(a: A, b: B)
}
