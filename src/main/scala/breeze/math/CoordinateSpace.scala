package breeze.math

import breeze.linalg.support.{CanCreateZerosLike, CanCopy, CanZipMapValues, CanNorm}
import breeze.generic.CanMapValues
import breeze.linalg.operators._
import breeze.linalg.NumericOps

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
  implicit def norm: CanNorm[V, Double]
  implicit def mapValues: CanMapValues[V,S,S,V]
  implicit def zipMapValues: CanZipMapValues[V,S,S,V]

  implicit def addVS: BinaryOp[V, S, OpAdd, V]
  implicit def subVS: BinaryOp[V, S, OpSub, V]
  implicit def mulVV: BinaryOp[V, V, OpMulScalar, V]
  implicit def divVV: BinaryOp[V, V, OpDiv, V]
//  implicit def powVV: BinaryOp[V, V, OpPow, V]
//  implicit def powVS: BinaryOp[V, S, OpPow, V]
//  implicit def modVV: BinaryOp[V, V, OpMod, V]
//  implicit def modVS: BinaryOp[V, S, OpMod, V]
}

object CoordinateSpace {
  implicit def fromField[S](implicit f: Field[S], canNorm: CanNorm[S, Double]):CoordinateSpace[S, S] = new CoordinateSpace[S, S] {
    def field: Field[S] = f


    implicit def scalarNorm: CanNorm[S, Unit] = new CanNorm[S, Unit] {
      def apply(v1: S, v2: Unit): Double = canNorm(v1, 1)
    }

    implicit def isNumericOps(v: S): NumericOps[S] = new NumericOps[S] {
      def repr: S = v
    }

    implicit val zeros: CanCreateZerosLike[S, S] = new CanCreateZerosLike[S, S] {
      // Should not inherit from Form=>To because the compiler will try to use it to coerce types.
      def apply(from: S): S = f.zero
    }

    import BinaryOp._

    implicit val mulVS: BinaryOp[S, S, OpMulScalar, S] = scalarOpMul[S]

    implicit val divVS: BinaryOp[S, S, OpDiv, S] = scalarOpDiv

    implicit val addVV: BinaryOp[S, S, OpAdd, S] = scalarOpAdd

    implicit val subVV: BinaryOp[S, S, OpSub, S] = scalarOpSub

    implicit val neg: UnaryOp[S, OpNeg, S] = UnaryOp.scalaOpNeg

    implicit val norm: CanNorm[S, Double] = canNorm

    implicit val mapValues: CanMapValues[S, S, S, S] = CanMapValues.canMapSelf

    implicit val zipMapValues: CanZipMapValues[S, S, S, S] = CanZipMapValues.canZipMapSelf

    implicit val addVS: BinaryOp[S, S, OpAdd, S] = scalarOpAdd

    implicit val subVS: BinaryOp[S, S, OpSub, S] = scalarOpSub

    implicit val mulVV: BinaryOp[S, S, OpMulScalar, S] = scalarOpMul

    implicit val divVV: BinaryOp[S, S, OpDiv, S] = scalarOpDiv

    implicit val dotVV: BinaryOp[S, S, OpMulInner, S] = scalarOpMulInner
  }
}

trait MutableCoordinateSpace[V, S] extends MutableInnerProductSpace[V, S]  with CoordinateSpace[V, S] {
  implicit def addIntoVS: BinaryUpdateOp[V, S, OpAdd]
  implicit def subIntoVS: BinaryUpdateOp[V, S, OpSub]
  implicit def mulIntoVV: BinaryUpdateOp[V, V, OpMulScalar]
  implicit def divIntoVV: BinaryUpdateOp[V, V, OpDiv]
  implicit def setIntoVS: BinaryUpdateOp[V, S, OpSet]

//  implicit def powIntoVV: BinaryUpdateOp[V, V, OpPow]
//  implicit def powIntoVS: BinaryUpdateOp[V, S, OpPow]

//  implicit def modIntoVV: BinaryUpdateOp[V, V, OpMod]
//  implicit def modIntoVS: BinaryUpdateOp[V, S, OpMod]
}

object MutableCoordinateSpace {
  def make[V, S](implicit
                    _norm:  CanNorm[V, Double],
                    _scalarNorm:  CanNorm[S, Unit],
                   _mapValues:  CanMapValues[V, S, S, V],
//                   _reduce:  UReduceable[V, S],
                   _zipMapValues:  CanZipMapValues[V, S, S, V],
                   _addVS:  BinaryOp[V, S, OpAdd, V],
                   _subVS:  BinaryOp[V, S, OpSub, V],
                   _mulVV:  BinaryOp[V, V, OpMulScalar, V],
                   _divVV:  BinaryOp[V, V, OpDiv, V],
//                   _powVV:  BinaryOp[V, V, OpPow, V],
//                   _powVS:  BinaryOp[V, S, OpPow, V],
//                   _modVV:  BinaryOp[V, V, OpMod, V],
//                   _modVS:  BinaryOp[V, S, OpMod, V],
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
                  _field:  Field[S],
                   _zeros:  CanCreateZerosLike[V, V],
                   _mulVS:  BinaryOp[V, S, OpMulScalar, V],
                   _divVS:  BinaryOp[V, S, OpDiv, V],
                   _addVV:  BinaryOp[V, V, OpAdd, V],
                   _subVV:  BinaryOp[V, V, OpSub, V],
                   _neg:  UnaryOp[V, OpNeg, V],
                   _isNumericOps: V <:< NumericOps[V],
                   _axpy:  CanAxpy[S, V, V],
                   _dotVV:  BinaryOp[V, V, OpMulInner, S]):MutableCoordinateSpace[V, S] = new MutableCoordinateSpace[V, S] {
    implicit def norm: CanNorm[V, Double] = _norm

    implicit def scalarNorm: CanNorm[S, Unit] = _scalarNorm



    implicit def mapValues: CanMapValues[V, S, S, V] = _mapValues

//    implicit def reduce: UReduceable[V, S] = _reduce

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

    implicit def setIntoVV: BinaryUpdateOp[V, V, OpSet] = _setIntoVV

    implicit def setIntoVS: BinaryUpdateOp[V, S, OpSet] = _setIntoVS

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

    implicit def isNumericOps(v: V): NumericOps[V] = _isNumericOps(v)

    implicit def dotVV: BinaryOp[V, V, OpMulInner, S] = _dotVV

    implicit def axpyVV: CanAxpy[S, V, V] = _axpy
 }
}