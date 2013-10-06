package breeze.math

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import breeze.linalg.operators._
import breeze.linalg.support.{CanZipMapValues, CanNorm, CanCopy, CanCreateZerosLike}
import breeze.linalg.{QuasiTensor, TensorLike, Tensor, NumericOps}
import breeze.generic.{UReduceable, CanMapValues}

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
  implicit def mulVS_M: BinaryOp[V, S, OpMulMatrix, V] = mulVS.asInstanceOf[BinaryOp[V, S, OpMulMatrix, V]]
  implicit def divVS: BinaryOp[V, S, OpDiv, V]

  implicit def addVV: BinaryOp[V, V, OpAdd, V]
  implicit def subVV: BinaryOp[V, V, OpSub, V]

  def close(a: V, b: V, tolerance: Double):Boolean

  // default implementations
  implicit def neg: UnaryOp[V, OpNeg, V]

}

trait NormedVectorSpace[V, S] extends VectorSpace[V, S] {
  def norm(a: V):Double
  def close(a: V, b: V, tolerance: Double):Boolean = norm(a - b) <= tolerance * math.max(norm(a), norm(b))
}

trait InnerProductSpace[V, S] extends NormedVectorSpace[V, S] {
  implicit def dotVV: BinaryOp[V, V, OpMulInner, S]

  def norm(a: V) = math.sqrt(field.norm(dotVV(a,a)))
}

trait MutableVectorSpace[V, S] extends VectorSpace[V, S] {
  implicit def copy: CanCopy[V]
  implicit def mulIntoVS: BinaryUpdateOp[V, S, OpMulScalar]
  implicit def mulIntoVS_M: BinaryUpdateOp[V, S, OpMulMatrix] = mulIntoVS.asInstanceOf[BinaryUpdateOp[V, S, OpMulMatrix]]
  implicit def divIntoVS: BinaryUpdateOp[V, S, OpDiv]

  implicit def addIntoVV: BinaryUpdateOp[V, V, OpAdd]
  implicit def subIntoVV: BinaryUpdateOp[V, V, OpSub]

  implicit def setIntoVV: BinaryUpdateOp[V, V, OpSet]

  implicit def axpyVV: CanAxpy[S, V, V]
}

trait MutableNormedSpace[V, S] extends NormedVectorSpace[V, S] with MutableVectorSpace[V, S]
trait MutableInnerProductSpace[V, S] extends InnerProductSpace[V, S] with MutableVectorSpace[V, S]

object MutableInnerProductSpace {
  /** Construct a MutableInnerProductSpace for the given type from the available implicits */
  def make[V, I, S](implicit _field: Field[S],
    _isNumericOps: V <:< NumericOps[V],
    _zeros: CanCreateZerosLike[V, V],
    _mulVS: BinaryOp[V, S, OpMulScalar, V],
    _divVS: BinaryOp[V, S, OpDiv, V],
    _addVV: BinaryOp[V, V, OpAdd, V],
    _subVV: BinaryOp[V, V, OpSub, V],
    _neg: UnaryOp[V, OpNeg, V],
    _dotVV: BinaryOp[V, V, OpMulInner, S],
    _copy: CanCopy[V],
    _mulIntoVS: BinaryUpdateOp[V, S, OpMulScalar],
    _divIntoVS: BinaryUpdateOp[V, S, OpDiv],
    _addIntoVV: BinaryUpdateOp[V, V, OpAdd],
    _subIntoVV: BinaryUpdateOp[V, V, OpSub],
    _setIntoVV: BinaryUpdateOp[V, V, OpSet],
    _axpy: CanAxpy[S, V, V]): MutableInnerProductSpace[V, S] = new MutableInnerProductSpace[V, S] {
    def field: Field[S] = _field
    implicit def isNumericOps(v: V): NumericOps[V] = _isNumericOps(v)
    implicit def zeros: CanCreateZerosLike[V, V] = _zeros
    implicit def mulVS: BinaryOp[V, S, OpMulScalar, V] = _mulVS
    implicit def divVS: BinaryOp[V, S, OpDiv, V] = _divVS
    implicit def addVV: BinaryOp[V, V, OpAdd, V] = _addVV
    implicit def subVV: BinaryOp[V, V, OpSub, V] = _subVV
    implicit def neg: UnaryOp[V, OpNeg, V] = _neg
    implicit def dotVV: BinaryOp[V, V, OpMulInner, S] = _dotVV
    implicit def copy: CanCopy[V] = _copy
    implicit def mulIntoVS: BinaryUpdateOp[V, S, OpMulScalar] = _mulIntoVS
    implicit def divIntoVS: BinaryUpdateOp[V, S, OpDiv] = _divIntoVS
    implicit def addIntoVV: BinaryUpdateOp[V, V, OpAdd] = _addIntoVV
    implicit def subIntoVV: BinaryUpdateOp[V, V, OpSub] = _subIntoVV
    implicit def setIntoVV: BinaryUpdateOp[V, V, OpSet] = _setIntoVV
    implicit def axpyVV: CanAxpy[S, V, V] = _axpy
  }
}


trait TensorSpace[V, I, S] extends MutableCoordinateSpace[V, S] {
  implicit def isNumericOps(v: V):NumericOps[V] with QuasiTensor[I, S]
  implicit def reduce: UReduceable[V,S]

}

object TensorSpace {
  def make[V, I, S](implicit
                    _norm:  CanNorm[V],
                   _mapValues:  CanMapValues[V, S, S, V],
                   _reduce:  UReduceable[V, S],
                   _zipMapValues:  CanZipMapValues[V, S, S, V],
                   _addVS:  BinaryOp[V, S, OpAdd, V],
                   _subVS:  BinaryOp[V, S, OpSub, V],
                   _mulVV:  BinaryOp[V, V, OpMulScalar, V],
                   _divVV:  BinaryOp[V, V, OpDiv, V],
                   _copy:  CanCopy[V],
                   _mulIntoVS:  BinaryUpdateOp[V, S, OpMulScalar],
                   _divIntoVS:  BinaryUpdateOp[V, S, OpDiv],
                   _addIntoVV:  BinaryUpdateOp[V, V, OpAdd],
                   _subIntoVV:  BinaryUpdateOp[V, V, OpSub],
                   _addIntoVS:  BinaryUpdateOp[V, S, OpAdd],
                   _subIntoVS:  BinaryUpdateOp[V, S, OpSub],
                   _mulIntoVV:  BinaryUpdateOp[V, V, OpMulScalar],
                   _divIntoVV:  BinaryUpdateOp[V, V, OpDiv],
                   _setIntoVV:  BinaryUpdateOp[V, V, OpSet],
                   _setIntoVS:  BinaryUpdateOp[V, S, OpSet],
//                   _powIntoVV:  BinaryUpdateOp[V, V, OpPow],
//                   _powIntoVS:  BinaryUpdateOp[V, S, OpPow],
//                   _modIntoVV:  BinaryUpdateOp[V, V, OpMod],
//                   _modIntoVS:  BinaryUpdateOp[V, S, OpMod],
                   _axpy:  CanAxpy[S, V, V],
                   _field:  Field[S],
                   _zeros:  CanCreateZerosLike[V, V],
                   _mulVS:  BinaryOp[V, S, OpMulScalar, V],
                   _divVS:  BinaryOp[V, S, OpDiv, V],
                   _addVV:  BinaryOp[V, V, OpAdd, V],
                   _subVV:  BinaryOp[V, V, OpSub, V],
                   _neg:  UnaryOp[V, OpNeg, V],
                   _isNumericOps: V <:< NumericOps[V] with QuasiTensor[I, S],
                   _dotVV:  BinaryOp[V, V, OpMulInner, S]):TensorSpace[V, I, S] = new TensorSpace[V, I, S] {
    implicit def norm: CanNorm[V] = _norm

    implicit def mapValues: CanMapValues[V, S, S, V] = _mapValues

    implicit def reduce: UReduceable[V, S] = _reduce

    implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapValues

    implicit def addVS: BinaryOp[V, S, OpAdd, V] = _addVS

    implicit def subVS: BinaryOp[V, S, OpSub, V] = _subVS

    implicit def mulVV: BinaryOp[V, V, OpMulScalar, V] = _mulVV

    implicit def divVV: BinaryOp[V, V, OpDiv, V] = _divVV

//    implicit def powVV: BinaryOp[V, V, OpPow, V] = _powVV
//
//    implicit def powVS: BinaryOp[V, S, OpPow, V] = _powVS
//
//    implicit def modVV: BinaryOp[V, V, OpMod, V] = _modVV
//
//    implicit def modVS: BinaryOp[V, S, OpMod, V] = _modVS

    implicit def copy: CanCopy[V] = _copy

    implicit def mulIntoVS: BinaryUpdateOp[V, S, OpMulScalar] = _mulIntoVS

    implicit def divIntoVS: BinaryUpdateOp[V, S, OpDiv] = _divIntoVS

    implicit def addIntoVV: BinaryUpdateOp[V, V, OpAdd] = _addIntoVV

    implicit def subIntoVV: BinaryUpdateOp[V, V, OpSub] = _subIntoVV

    implicit def addIntoVS: BinaryUpdateOp[V, S, OpAdd] = _addIntoVS

    implicit def subIntoVS: BinaryUpdateOp[V, S, OpSub] = _subIntoVS

    implicit def mulIntoVV: BinaryUpdateOp[V, V, OpMulScalar] = _mulIntoVV

    implicit def divIntoVV: BinaryUpdateOp[V, V, OpDiv] = _divIntoVV

//    implicit def powIntoVV: BinaryUpdateOp[V, V, OpPow] = _powIntoVV
//
//    implicit def powIntoVS: BinaryUpdateOp[V, S, OpPow] = _powIntoVS
//
//    implicit def modIntoVV: BinaryUpdateOp[V, V, OpMod] = _modIntoVV
//
//    implicit def modIntoVS: BinaryUpdateOp[V, S, OpMod] = _modIntoVS

    def field: Field[S] = _field

    implicit def zeros: CanCreateZerosLike[V, V] = _zeros

    implicit def mulVS: BinaryOp[V, S, OpMulScalar, V] = _mulVS

    implicit def divVS: BinaryOp[V, S, OpDiv, V] = _divVS

    implicit def addVV: BinaryOp[V, V, OpAdd, V] = _addVV

    implicit def subVV: BinaryOp[V, V, OpSub, V] = _subVV

    implicit def neg: UnaryOp[V, OpNeg, V] = _neg

    override def norm(a: V): Double = norm.apply(a, 2)

    implicit def isNumericOps(v: V): NumericOps[V] with QuasiTensor[I, S] = _isNumericOps(v)

    implicit def dotVV: BinaryOp[V, V, OpMulInner, S] = _dotVV


    implicit def setIntoVV: BinaryUpdateOp[V, V, OpSet] = _setIntoVV

    implicit def setIntoVS: BinaryUpdateOp[V, S, OpSet] = _setIntoVS

    implicit def axpyVV: CanAxpy[S, V, V] = _axpy
  }
}
