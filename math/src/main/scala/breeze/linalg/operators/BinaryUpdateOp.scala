package breeze.linalg.operators
import breeze.generic.Multiproc2


trait BinaryUpdateOp[A, B, Op<:OpType] extends ((A, B) => Unit) {
  def apply(a: A, b: B)
}


object BinaryUpdateOp {
  type Bind[Op <:OpType] = { type Sig[A, B] = BinaryUpdateOp[A, B, Op]}
}


trait BinaryUpdateRegistry[A<:AnyRef, B, Op<:OpType] extends BinaryUpdateOp[A, B, Op] with Multiproc2[BinaryUpdateOp.Bind[Op]#Sig, A, B] {
}
