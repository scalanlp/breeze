package breeze.math

import breeze.linalg.operators._
import breeze.linalg.support.{CanZipMapValues, CanNorm, CanCopy, CanCreateZerosLike}
import breeze.linalg.{QuasiTensor, TensorLike, Tensor, NumericOps}
import breeze.generic.CanMapValues

/**
 *
 * @tparam V Vector type
 * @tparam S Scalar type
 * @author dlwh
 */
trait VectorSpace[V, S] {
  def field: Field[S]

  implicit def isNumericOps(v: V):NumericOps[V]

  implicit def zeros: CanCreateZerosLike[V, V]
  implicit def mulVS: BinaryOp[V, S, OpMulScalar, V]
  implicit def divVS: BinaryOp[V, S, OpDiv, V]

  implicit def addVV: BinaryOp[V, V, OpAdd, V]
  implicit def subVV: BinaryOp[V, V, OpSub, V]

  def close(a: V, b: V, tolerance: Double):Boolean

  // default implementations
  implicit def neg: UnaryOp[V, OpNeg, V]

}

trait NormedVectorSpace[V, S] extends VectorSpace[V, S] {
  def norm(a: V):Double
  def close(a: V, b: V, tolerance: Double):Boolean = norm(a - b) < tolerance
}

trait InnerProductSpace[V, S] extends NormedVectorSpace[V, S] {
  implicit def dotVV: BinaryOp[V, V, OpMulInner, Double]
}

trait MutableVectorSpace[V, S] extends VectorSpace[V, S] {
  implicit def copy: CanCopy[V]
  implicit def mulIntoVS: BinaryUpdateOp[V, S, OpMulScalar]
  implicit def divIntoVS: BinaryUpdateOp[V, S, OpDiv]

  implicit def addIntoVV: BinaryUpdateOp[V, V, OpAdd]
  implicit def subIntoVV: BinaryUpdateOp[V, V, OpSub]
}

trait MutableNormedSpace[V, S] extends NormedVectorSpace[V, S] with MutableVectorSpace[V, S]
trait MutableInnerProductSpace[V, S] extends InnerProductSpace[V, S] with MutableVectorSpace[V, S]


/**
 * A coordinate space is like a [[breeze.math.InnerProductSpace]], but
 * it supports the full suite of "Tensor-y" operations. The intuition
 * is that anything that can work on a Tensor/Vector will work here.
 *
 * For example V + S doesn't work in a vector space, but it does in
 * a coordinate space.
 *
 * @tparam V
 * @tparam S
 */
trait CoordinateSpace[V, S] extends InnerProductSpace[V, S] {
  implicit def norm: CanNorm[V]
  implicit def mapValues: CanMapValues[V,S,S,V]
  implicit def zipMapValues: CanZipMapValues[V,S,S,V]

  implicit def addVS: BinaryOp[V, S, OpAdd, V]
  implicit def subVS: BinaryOp[V, S, OpSub, V]
  implicit def mulVV: BinaryOp[V, V, OpMulScalar, V]
  implicit def divVV: BinaryOp[V, V, OpDiv, V]
  implicit def powVV: BinaryOp[V, V, OpPow, V]
  implicit def powVS: BinaryOp[V, S, OpPow, V]
  implicit def modVV: BinaryOp[V, V, OpMod, V]
  implicit def modVS: BinaryOp[V, S, OpMod, V]
}

trait MutableCoordinateSpace[V, S] extends MutableInnerProductSpace[V, S]  with CoordinateSpace[V, S] {
  implicit def addIntoVS: BinaryUpdateOp[V, S, OpAdd]
  implicit def subIntoVS: BinaryUpdateOp[V, S, OpSub]
  implicit def mulIntoVV: BinaryUpdateOp[V, V, OpMulScalar]
  implicit def divIntoVV: BinaryUpdateOp[V, V, OpDiv]

  implicit def powIntoVV: BinaryUpdateOp[V, V, OpPow]
  implicit def powIntoVS: BinaryUpdateOp[V, S, OpPow]

  implicit def modIntoVV: BinaryUpdateOp[V, V, OpMod]
  implicit def modIntoVS: BinaryUpdateOp[V, S, OpMod]
}

trait TensorSpace[V, I, S] extends MutableCoordinateSpace[V, S] {
  implicit def isNumericOps(v: V):NumericOps[V] with QuasiTensor[I, S]

}
