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
import breeze.linalg.support.{CanZipMapValues, CanCopy, CanCreateZerosLike}
import breeze.linalg.{norm, QuasiTensor, NumericOps}
import breeze.generic.{CanTraverseValues, CanMapValues}

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
  implicit def mulVS: OpMulScalar.Impl2[V, S, V]
  implicit def mulVS_M: OpMulMatrix.Impl2[V, S, V] = mulVS.asInstanceOf[OpMulMatrix.Impl2[V, S, V]]
  implicit def divVS: OpDiv.Impl2[V, S, V]

  implicit def addVV: OpAdd.Impl2[V, V, V]
  implicit def subVV: OpSub.Impl2[V, V, V]

  def close(a: V, b: V, tolerance: Double):Boolean

  // default implementations
  implicit def neg: OpNeg.Impl[V, V]

}

trait NormedVectorSpace[V, S] extends VectorSpace[V, S] {
  implicit def normImpl: norm.Impl[V, Double]
  def close(a: V, b: V, tolerance: Double):Boolean = norm(a - b) <= tolerance * math.max(norm(a), norm(b))
}

trait InnerProductSpace[V, S] extends NormedVectorSpace[V, S] {
  implicit def dotVV: OpMulInner.Impl2[V, V, S]

  implicit def scalarNorm: norm.Impl[S, Double]

  implicit def normImpl: norm.Impl[V, Double] = new norm.Impl[V, Double] {
    def apply(v: V): Double = math.sqrt(scalarNorm(dotVV(v, v)))
  }
}

trait MutableVectorSpace[V, S] extends VectorSpace[V, S] {
  implicit def copy: CanCopy[V]
  implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S]
  implicit def mulIntoVS_M: OpMulMatrix.InPlaceImpl2[V, S] = mulIntoVS.asInstanceOf[OpMulMatrix.InPlaceImpl2[V, S]]
  implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S]

  implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V]
  implicit def subIntoVV: OpSub.InPlaceImpl2[V, V]

  implicit def setIntoVV: OpSet.InPlaceImpl2[V, V]

  implicit def axpyVV: CanAxpy[S, V, V]
}

trait MutableNormedSpace[V, S] extends NormedVectorSpace[V, S] with MutableVectorSpace[V, S]
trait MutableInnerProductSpace[V, S] extends InnerProductSpace[V, S] with MutableVectorSpace[V, S]

object MutableInnerProductSpace {
  /** Construct a MutableInnerProductSpace for the given type from the available implicits */
  def make[V, I, S](implicit _field: Field[S],
                    _scalarNorm: norm.Impl[S, Double],
    _isNumericOps: V <:< NumericOps[V],
    _zeros: CanCreateZerosLike[V, V],
    _mulVS: OpMulScalar.Impl2[V, S, V],
    _divVS: OpDiv.Impl2[V, S, V],
    _addVV: OpAdd.Impl2[V, V, V],
    _subVV: OpSub.Impl2[V, V, V],
    _neg: OpNeg.Impl[V, V],
    _dotVV: OpMulInner.Impl2[V, V, S],
    _copy: CanCopy[V],
    _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
    _divIntoVS: OpDiv.InPlaceImpl2[V, S],
    _addIntoVV: OpAdd.InPlaceImpl2[V, V],
    _subIntoVV: OpSub.InPlaceImpl2[V, V],
    _setIntoVV: OpSet.InPlaceImpl2[V, V],
    _axpy: CanAxpy[S, V, V]): MutableInnerProductSpace[V, S] = new MutableInnerProductSpace[V, S] {
    def field: Field[S] = _field

    implicit def scalarNorm = _scalarNorm

    implicit def isNumericOps(v: V): NumericOps[V] = _isNumericOps(v)
    implicit def zeros: CanCreateZerosLike[V, V] = _zeros
    implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    implicit def neg: OpNeg.Impl[V, V] = _neg
    implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    implicit def copy: CanCopy[V] = _copy
    implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
    implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    implicit def axpyVV: CanAxpy[S, V, V] = _axpy
  }
}


trait TensorSpace[V, I, S] extends MutableCoordinateSpace[V, S] {
  implicit def isNumericOps(v: V):NumericOps[V] with QuasiTensor[I, S]
  implicit def iterateValues: CanTraverseValues[V,S]

}

object TensorSpace {
  def make[V, I, S](implicit
                    _norm:  norm.Impl2[V, Double, Double],
                    _scalarNorm: norm.Impl[S, Double] ,
                   _mapValues:  CanMapValues[V, S, S, V],
                   _reduce:  CanTraverseValues[V, S],
                   _zipMapValues:  CanZipMapValues[V, S, S, V],
                   _addVS:  OpAdd.Impl2[V, S, V],
                   _subVS:  OpSub.Impl2[V, S, V],
                   _mulVV:  OpMulScalar.Impl2[V, V, V],
                   _divVV:  OpDiv.Impl2[V, V, V],
                   _copy:  CanCopy[V],
                   _mulIntoVS:  OpMulScalar.InPlaceImpl2[V, S],
                   _divIntoVS:  OpDiv.InPlaceImpl2[V, S],
                   _addIntoVV:  OpAdd.InPlaceImpl2[V, V],
                   _subIntoVV:  OpSub.InPlaceImpl2[V, V],
                   _addIntoVS:  OpAdd.InPlaceImpl2[V, S],
                   _subIntoVS:  OpSub.InPlaceImpl2[V, S],
                   _mulIntoVV:  OpMulScalar.InPlaceImpl2[V, V],
                   _divIntoVV:  OpDiv.InPlaceImpl2[V, V],
                   _setIntoVV:  OpSet.InPlaceImpl2[V, V],
                   _setIntoVS:  OpSet.InPlaceImpl2[V, S],
//                   _powIntoVV:  OpPow.InPlaceImpl2[V, V],
//                   _powIntoVS:  OpPow.InPlaceImpl2[V, S],
//                   _modIntoVV:  OpMod.InPlaceImpl2[V, V],
//                   _modIntoVS:  OpMod.InPlaceImpl2[V, S],
                   _axpy:  CanAxpy[S, V, V],
                   _field:  Field[S],
                   _zeros:  CanCreateZerosLike[V, V],
                   _mulVS:  OpMulScalar.Impl2[V, S, V],
                   _divVS:  OpDiv.Impl2[V, S, V],
                   _addVV:  OpAdd.Impl2[V, V, V],
                   _subVV:  OpSub.Impl2[V, V, V],
                   _neg:  OpNeg.Impl[V, V],
                   _isNumericOps: V <:< NumericOps[V] with QuasiTensor[I, S],
                   _dotVV:  OpMulInner.Impl2[V, V, S]):TensorSpace[V, I, S] = new TensorSpace[V, I, S] {
    implicit def normImplDouble: norm.Impl2[V, Double, Double] = _norm

    implicit def scalarNorm: norm.Impl[S, Double] = _scalarNorm
    implicit def mapValues: CanMapValues[V, S, S, V] = _mapValues

    implicit def iterateValues: CanTraverseValues[V, S] = _reduce

    implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapValues

    implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS

    implicit def subVS: OpSub.Impl2[V, S, V] = _subVS

    implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV

    implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV

//    implicit def powVV: OpPow.Impl2[V, V, V] = _powVV
//
//    implicit def powVS: OpPow.Impl2[V, S, V] = _powVS
//
//    implicit def modVV: OpMod.Impl2[V, V, V] = _modVV
//
//    implicit def modVS: OpMod.Impl2[V, S, V] = _modVS

    implicit def copy: CanCopy[V] = _copy

    implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS

    implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS

    implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV

    implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV

    implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S] = _addIntoVS

    implicit def subIntoVS: OpSub.InPlaceImpl2[V, S] = _subIntoVS

    implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV

    implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV

//    implicit def powIntoVV: OpPow.InPlaceImpl2[V, V] = _powIntoVV
//
//    implicit def powIntoVS: OpPow.InPlaceImpl2[V, S] = _powIntoVS
//
//    implicit def modIntoVV: OpMod.InPlaceImpl2[V, V] = _modIntoVV
//
//    implicit def modIntoVS: OpMod.InPlaceImpl2[V, S] = _modIntoVS

    def field: Field[S] = _field

    implicit def zeros: CanCreateZerosLike[V, V] = _zeros

    implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS

    implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS

    implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV

    implicit def subVV: OpSub.Impl2[V, V, V] = _subVV

    implicit def neg: OpNeg.Impl[V, V] = _neg

    implicit def isNumericOps(v: V): NumericOps[V] with QuasiTensor[I, S] = _isNumericOps(v)

    implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV


    implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV

    implicit def setIntoVS: OpSet.InPlaceImpl2[V, S] = _setIntoVS

    implicit def axpyVV: CanAxpy[S, V, V] = _axpy
  }
}
