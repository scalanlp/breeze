package breeze.math

import breeze.linalg.support._
import breeze.linalg.operators._
import breeze.linalg.{norm, NumericOps}

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
  implicit def normImplDouble: norm.Impl2[V, Double, Double]
  implicit def mapValues: CanMapValues[V,S,S,V]
  implicit def zipMapValues: CanZipMapValues[V,S,S,V]

  implicit def addVS: OpAdd.Impl2[V, S, V]
  implicit def subVS: OpSub.Impl2[V, S, V]
  implicit def mulVV: OpMulScalar.Impl2[V, V, V]
  implicit def divVV: OpDiv.Impl2[V, V, V]
//  implicit def powVV: OpPow.Impl2[V, V, V]
//  implicit def powVS: OpPow.Impl2[V, S, V]
//  implicit def modVV: OpMod.Impl2[V, V, V]
//  implicit def modVS: OpMod.Impl2[V, S, V]
}

object CoordinateSpace {
  implicit def fromField[S](implicit f: Field[S], _normImpl: norm.Impl[S, Double]):CoordinateSpace[S, S] = new CoordinateSpace[S, S] {
    def field: Field[S] = f


    implicit def scalarNorm = _normImpl

    implicit def isNumericOps(v: S): NumericOps[S] = new NumericOps[S] {
      def repr: S = v
    }

    implicit val zeros: CanCreateZerosLike[S, S] = new CanCreateZerosLike[S, S] {
      // Should not inherit from Form=>To because the compiler will try to use it to coerce types.
      def apply(from: S): S = f.zero
    }

    import BinaryOp._

    implicit val mulVS: OpMulScalar.Impl2[S, S, S] = OpMulScalar.opMulScalarFromSemiring[S]

    implicit val divVS: OpDiv.Impl2[S, S, S] = OpDiv.opDivFromField[S]

    implicit val addVV: OpAdd.Impl2[S, S, S] = OpAdd.opAddFromSemiring

    implicit val subVV: OpSub.Impl2[S, S, S] = OpSub.opSubFromRing

    implicit val neg: OpNeg.Impl[S, S] = OpNeg.ringNegation

    implicit val normImplDouble:norm.Impl2[S, Double, Double] = new norm.Impl2[S, Double, Double] {
      def apply(v: S, v2: Double): Double = scalarNorm(v)
    }

    implicit val mapValues: CanMapValues[S, S, S, S] = CanMapValues.canMapSelf

    implicit val zipMapValues: CanZipMapValues[S, S, S, S] = CanZipMapValues.canZipMapSelf

    implicit val addVS: OpAdd.Impl2[S, S, S] = OpAdd.opAddFromSemiring

    implicit val subVS: OpSub.Impl2[S, S, S] = OpSub.opSubFromRing

    implicit val mulVV: OpMulScalar.Impl2[S, S, S] = OpMulScalar.opMulScalarFromSemiring

    implicit val divVV: OpDiv.Impl2[S, S, S] = OpDiv.opDivFromField

    implicit val dotVV: OpMulInner.Impl2[S, S, S] = OpMulInner.opMulInnerFromSemiring
  }
}

trait MutableCoordinateSpace[V, S] extends MutableInnerProductSpace[V, S]  with CoordinateSpace[V, S] {
  implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S]
  implicit def subIntoVS: OpSub.InPlaceImpl2[V, S]
  implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V]
  implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V]
  implicit def setIntoVS: OpSet.InPlaceImpl2[V, S]

//  implicit def powIntoVV: OpPow.InPlaceImpl2[V, V]
//  implicit def powIntoVS: OpPow.InPlaceImpl2[V, S]

//  implicit def modIntoVV: OpMod.InPlaceImpl2[V, V]
//  implicit def modIntoVS: OpMod.InPlaceImpl2[V, S]
}

object MutableCoordinateSpace {
  def make[V, S](implicit
                    _norm:  norm.Impl2[V, Double, Double],
                    _scalarNorm:  norm.Impl[S, Double],
                   _mapValues:  CanMapValues[V, S, S, V],
//                   _reduce:  UReduceable[V, S],
                   _zipMapValues:  CanZipMapValues[V, S, S, V],
                   _addVS:  OpAdd.Impl2[V, S, V],
                   _subVS:  OpSub.Impl2[V, S, V],
                   _mulVV:  OpMulScalar.Impl2[V, V, V],
                   _divVV:  OpDiv.Impl2[V, V, V],
//                   _powVV:  OpPow.Impl2[V, V, V],
//                   _powVS:  OpPow.Impl2[V, S, V],
//                   _modVV:  OpMod.Impl2[V, V, V],
//                   _modVS:  OpMod.Impl2[V, S, V],
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
                  _field:  Field[S],
                   _zeros:  CanCreateZerosLike[V, V],
                   _mulVS:  OpMulScalar.Impl2[V, S, V],
                   _divVS:  OpDiv.Impl2[V, S, V],
                   _addVV:  OpAdd.Impl2[V, V, V],
                   _subVV:  OpSub.Impl2[V, V, V],
                   _neg:  OpNeg.Impl[V, V],
                   _isNumericOps: V <:< NumericOps[V],
                   _axpy:  CanAxpy[S, V, V],
                   _dotVV:  OpMulInner.Impl2[V, V, S]):MutableCoordinateSpace[V, S] = new MutableCoordinateSpace[V, S] {
    implicit def normImplDouble = _norm

    implicit def scalarNorm: norm.Impl[S, Double] = _scalarNorm

    implicit def mapValues: CanMapValues[V, S, S, V] = _mapValues

//    implicit def reduce: UReduceable[V, S] = _reduce

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

    implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV

    implicit def setIntoVS: OpSet.InPlaceImpl2[V, S] = _setIntoVS

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

    implicit def isNumericOps(v: V): NumericOps[V] = _isNumericOps(v)

    implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV

    implicit def axpyVV: CanAxpy[S, V, V] = _axpy
 }
}