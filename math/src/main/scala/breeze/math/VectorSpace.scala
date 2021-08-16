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

import breeze.generic.UFunc
import breeze.generic.UFunc.{InPlaceImpl2, InPlaceImpl3, UImpl2}
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.linalg._
import breeze.math.FloatDoubleOperatorAdaptors._
import breeze.storage._

import scala.reflect.ClassTag
import breeze.compat._
import breeze.compat.Scala3Compat._

/**
 * Used for those vector-types that are "coordinated", meaning that t. (Possibly the coordinates
 * are enumerable as well, in which case you want an XXX
 * @tparam V Vector type
 * @tparam S Scalar type
 * @author gabeos, dlwh
 */
trait Coordinated[V, S] {
  implicit def scalarOf: ScalarOf[V, S]
  implicit def mapValues: CanMapValues[V, S, S, V]
  implicit def zipMapValues: CanZipMapValues[V, S, S, V]
  implicit def iterateValues: CanTraverseValues[V, S]
}

trait AdditiveTensorAbelianGroup[V, S] {
  implicit def scalars: Semiring[S]
  implicit def addVV: OpAdd.Impl2[V, V, V] // Abelian Group operator (addition)
}

trait Normed[V] {
  implicit def normImpl: norm.Impl[V, Double]
}

/**
 * Has a norm(v, p), for real p (technically for p >= 1)
 * @tparam V
 */
trait PNormed[V] extends Normed[V] {
  implicit def normImpl2: norm.Impl2[V, Double, Double]
}

trait Module[V, S] extends AdditiveTensorAbelianGroup[V, S] {
  implicit def scalars: Ring[S]

  // Extra operations that are defined over ring derived from abelian group addition
  // e.g. scalar additive inverse + abelian group addition
  implicit def subVV: OpSub.Impl2[V, V, V]
  implicit def zeroLike: CanCreateZerosLike[V, V]

  // Module operator
  implicit def mulVS: OpMulScalar.Impl2[V, S, V]
  implicit def mulVS_M: OpMulMatrix.Impl2[V, S, V] = mulVS.asInstanceOf[OpMulMatrix.Impl2[V, S, V]]

  // Brings NumericOps into scope
  implicit val hasOps: ConversionOrSubtype[V, NumericOps[V]]

  def close(a: V, b: V, tolerance: Double): Boolean
}

// Modules
trait MutableModule[V, S] extends Module[V, S] {
  implicit def copy: CanCopy[V]
  implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S]
  implicit def mulIntoVS_M: OpMulMatrix.InPlaceImpl2[V, S] = mulIntoVS.asInstanceOf[OpMulMatrix.InPlaceImpl2[V, S]]
  implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V]
  implicit def subIntoVV: OpSub.InPlaceImpl2[V, V]
  implicit def setIntoVV: OpSet.InPlaceImpl2[V, V]
  implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V]
}

trait NormedModule[V, S] extends Module[V, S] with Normed[V] {
  implicit def scalarNorm: norm.Impl[S, Double] = scalars.normImpl
  def close(a: V, b: V, tolerance: Double): Boolean = {
    norm(subVV(a, b)) <= tolerance * math.max(1.0, math.max(norm(a), norm(b)))
  }
}

trait MutableNormedModule[V, S] extends MutableModule[V, S] with NormedModule[V, S]

/**
 * An LP Module is a module equipped with a p-norm (named after LPSpace)
 * @tparam V
 * @tparam S
 */
trait LPModule[V, S] extends NormedModule[V, S] with PNormed[V]

trait MutableLPModule[V, S] extends MutableModule[V, S] with LPModule[V, S]

trait InnerProductModule[V, S] extends NormedModule[V, S] {
  implicit def dotVV: OpMulInner.Impl2[V, V, S]
  implicit def normImpl: norm.Impl[V, Double] = new norm.Impl[V, Double] {
    def apply(v: V): Double = math.sqrt(scalars.sNorm(dotVV(v, v)))
  }
}

trait MutableInnerProductModule[V, S] extends MutableModule[V, S] with InnerProductModule[V, S]

// Vector Spaces
trait VectorSpace[V, S] extends Module[V, S] {
  implicit def scalars: Field[S]
  implicit def divVS: OpDiv.Impl2[V, S, V] // Inverse module operator since Fields have multiplicative inverse
}

trait MutableVectorSpace[V, S] extends MutableModule[V, S] with VectorSpace[V, S] {
  implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S]
}

trait NormedVectorSpace[V, S] extends VectorSpace[V, S] with NormedModule[V, S]

trait MutableNormedVectorSpace[V, S] extends MutableVectorSpace[V, S] with NormedVectorSpace[V, S]

trait LPSpace[V, S] extends VectorSpace[V, S] with LPModule[V, S]

trait MutableLPSpace[V, S] extends MutableVectorSpace[V, S] with MutableLPModule[V, S]

trait InnerProductVectorSpace[V, S] extends NormedVectorSpace[V, S] with InnerProductModule[V, S]

trait MutableInnerProductVectorSpace[V, S]
    extends MutableVectorSpace[V, S]
    with MutableInnerProductModule[V, S]
    with InnerProductVectorSpace[V, S]

// Groups over vectors under element-wise operations.
// e.g. VectorField is a Field of Vectors under element-wise addition, negation, multiplication and inversion.
//      Under the corresponding Matrix operations, vectors no longer form a Field
trait VectorRing[V, S] extends InnerProductModule[V, S] {
  implicit def mulVV: OpMulScalar.Impl2[V, V, V]
  implicit def neg: OpNeg.Impl[V, V]
}

trait MutableVectorRing[V, S] extends VectorRing[V, S] with MutableInnerProductModule[V, S] {
  implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V]
}

trait VectorField[V, S] extends InnerProductVectorSpace[V, S] with VectorRing[V, S] {
  implicit def divVV: OpDiv.Impl2[V, V, V]
}

trait MutableVectorField[V, S]
    extends VectorField[V, S]
    with MutableVectorRing[V, S]
    with MutableInnerProductVectorSpace[V, S] {
  implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V]
}

/** A [[breeze.math.VectorField]] and a [[breeze.math.LPSpace]] */
trait LPVectorField[V, S] extends VectorField[V, S] with LPSpace[V, S]

trait MutableLPVectorField[V, S]
    extends LPVectorField[V, S]
    with MutableVectorRing[V, S]
    with MutableInnerProductVectorSpace[V, S] {
  implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V]
}

// Same idea as VectorField, but with explicit key type specified.
trait CoordinateField[V, S] extends LPVectorField[V, S] with Coordinated[V, S] {}

trait MutableCoordinateField[V, S] extends CoordinateField[V, S] with MutableVectorField[V, S]

/**
 * A CoordinateField that has an addressable index set. This set may not be finite, and it may
 * change (e.g. Counters).
 *
 * Brings QuasiTensor methods into scope.
 * @tparam V
 * @tparam I
 * @tparam S
 */
trait EnumeratedCoordinateField[V, I, S] extends CoordinateField[V, S] {
  implicit val hasOps: ConversionOrSubtype[V, NumericOps[V] with QuasiTensor[I, S]]

  implicit def zipMapKeyValues: CanZipMapKeyValues[V, I, S, S, V]
}

/**
 * A CoordinateField that has an addressable index set. This set may not be finite, and it may
 * change (e.g. Counters).
 *
 * Brings QuasiTensor methods into scope.
 * @tparam V
 * @tparam I
 * @tparam S
 */
trait MutableEnumeratedCoordinateField[V, I, S]
    extends EnumeratedCoordinateField[V, I, S]
    with MutableCoordinateField[V, S]

/**
 * [[breeze.math.CoordinateField]] with generic zeros operation. Only useful for the Matrix
 * and Vector hierarchies where the domain can be specified by the dimension of the Tensor.
 *
 *
 * @author gabeos, dlwh
 */
trait FiniteCoordinateField[V, I, S] extends EnumeratedCoordinateField[V, I, S] {
  implicit def zero: CanCreateZeros[V, I]
  implicit def canDim: dim.Impl[V, I]

  implicit def addVS: OpAdd.Impl2[V, S, V] // Implicitly Broadcast scalars to vector-space
  implicit def subVS: OpSub.Impl2[V, S, V]
}

trait MutableFiniteCoordinateField[V, I, S]
    extends FiniteCoordinateField[V, I, S]
    with MutableEnumeratedCoordinateField[V, I, S] {

  implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S]
  implicit def subIntoVS: OpSub.InPlaceImpl2[V, S]
  implicit def setIntoVS: OpSet.InPlaceImpl2[V, S]
}

trait MutableOptimizationSpace[M, V, S] extends MutableFiniteCoordinateField[V, Int, S] {
  def toMatrix(v: V): M
  def toVector(m: M): V
  def closeM(a: M, b: M, tolerance: Double): Boolean
  implicit def normImpl2: norm.Impl2[V, Double, Double]
  implicit def fieldNorm: norm.Impl[S, Double]
  implicit def mulMMS: OpMulScalar.Impl2[M, M, M]
  implicit def mulMMM: OpMulMatrix.Impl2[M, M, M]
  implicit def mulMVV: OpMulMatrix.Impl2[M, V, V]
  implicit def mulVTM: OpMulMatrix.Impl2[V, Transpose[V], M]
  implicit def canTrans: CanTranspose[V, Transpose[V]]
  implicit def negM: OpNeg.Impl[M, M]
  implicit def zeroM: CanCreateZeros[M, (Int, Int)]
  implicit def canDimM: dim.Impl[M, (Int, Int)]
  implicit def hasMOps: ConversionOrSubtype[M, NumericOps[M] with QuasiTensor[(Int, Int), S]]
  implicit def normMImpl2: norm.Impl2[M, Double, Double]
  implicit def normM: norm.Impl[M, Double]
  implicit def divMM: OpDiv.Impl2[M, M, M]
  implicit def subMS: OpSub.Impl2[M, S, M]
  implicit def subMM: OpSub.Impl2[M, M, M]
  implicit def mulMS: OpMulScalar.Impl2[M, S, M]
  implicit def mulMSMat: OpMulMatrix.Impl2[M, S, M]
  implicit def zeroLikeM: CanCreateZerosLike[M, M]
  implicit def addMS: OpAdd.Impl2[M, S, M]
  implicit def addMM: OpAdd.Impl2[M, M, M]
  implicit def divMS: OpDiv.Impl2[M, S, M]
  implicit def dotMM: OpMulInner.Impl2[M, M, S]
  implicit def divIntoMM: OpDiv.InPlaceImpl2[M, M]
  implicit def divIntoMS: OpDiv.InPlaceImpl2[M, S]
  implicit def copyM: CanCopy[M]
  implicit def mulIntoMS: OpMulScalar.InPlaceImpl2[M, S]
  implicit def addIntoMM: OpAdd.InPlaceImpl2[M, M]
  implicit def subIntoMM: OpSub.InPlaceImpl2[M, M]
  implicit def addIntoMS: OpAdd.InPlaceImpl2[M, S]
  implicit def subIntoMS: OpSub.InPlaceImpl2[M, S]
  implicit def setIntoMM: OpSet.InPlaceImpl2[M, M]
  implicit def scaleAddMM: scaleAdd.InPlaceImpl3[M, S, M]
  implicit def setIntoMS: OpSet.InPlaceImpl2[M, S]
  implicit def mulIntoMM: OpMulScalar.InPlaceImpl2[M, M]
  implicit def scalarOfM: ScalarOf[M, S]
  implicit def mapValuesM: CanMapValues[M, S, S, M]
  implicit def zipMapValuesM: CanZipMapValues[M, S, S, M]
  implicit def iterateValuesM: CanTraverseValues[M, S]
}

object VectorField {
  def make[V, S](
      implicit
      _norm: norm.Impl[V, Double],
      _field: Field[S],
      _mulVV: OpMulScalar.Impl2[V, V, V],
      _divVV: OpDiv.Impl2[V, V, V],
      _zeroLike: CanCreateZerosLike[V, V],
      _mulVS: OpMulScalar.Impl2[V, S, V],
      _divVS: OpDiv.Impl2[V, S, V],
      _addVV: OpAdd.Impl2[V, V, V],
      _subVV: OpSub.Impl2[V, V, V],
      _dotVV: OpMulInner.Impl2[V, V, S],
      _neg: OpNeg.Impl[V, V],
      _ops: V <:< NumericOps[V]): VectorField[V, S] = new VectorField[V, S] {
    def scalars: Field[S] = _field
    override val hasOps: ConversionOrSubtype[V, NumericOps[V]] = implicitly
    override implicit def normImpl: norm.Impl[V, Double] = _norm
    override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
    override implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def neg: OpNeg.Impl[V, V] = _neg
  }

}

object MutableModule {

  /** Construct a MutableInnerProductSpace for the given type from the available implicits */
  def make[V, S](closeTo: (V, V, Double) => Boolean)(
      implicit _ring: Ring[S],
      _zeroLike: CanCreateZerosLike[V, V],
      _ops: V <:< NumericOps[V],
      _mulVS: OpMulScalar.Impl2[V, S, V],
      _addVV: OpAdd.Impl2[V, V, V],
      _subVV: OpSub.Impl2[V, V, V],
      _copy: CanCopy[V],
      _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
      _addIntoVV: OpAdd.InPlaceImpl2[V, V],
      _subIntoVV: OpSub.InPlaceImpl2[V, V],
      _setIntoVV: OpSet.InPlaceImpl2[V, V],
      _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V]): MutableModule[V, S] = new MutableModule[V, S] {
    def scalars: Ring[S] = _ring
    def close(a: V, b: V, tolerance: Double): Boolean = closeTo(a, b, tolerance)
    override val hasOps: ConversionOrSubtype[V, NumericOps[V]] = implicitly
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def copy: CanCopy[V] = _copy
    override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV

    override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
  }
}

object MutableInnerProductVectorSpace {

  /** Construct a MutableInnerProductSpace for the given type from the available implicits */
  def make[V, S](
      implicit _field: Field[S],
      _ops: V <:< NumericOps[V],
      _zeroLike: CanCreateZerosLike[V, V],
      _mulVS: OpMulScalar.Impl2[V, S, V],
      _divVS: OpDiv.Impl2[V, S, V],
      _addVV: OpAdd.Impl2[V, V, V],
      _subVV: OpSub.Impl2[V, V, V],
      _dotVV: OpMulInner.Impl2[V, V, S],
      _copy: CanCopy[V],
      _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
      _divIntoVS: OpDiv.InPlaceImpl2[V, S],
      _addIntoVV: OpAdd.InPlaceImpl2[V, V],
      _subIntoVV: OpSub.InPlaceImpl2[V, V],
      _setIntoVV: OpSet.InPlaceImpl2[V, V],
      _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V]): MutableInnerProductVectorSpace[V, S] =
    new MutableInnerProductVectorSpace[V, S] {
      def scalars: Field[S] = _field
      override val hasOps: ConversionOrSubtype[V, NumericOps[V]] = implicitly
      override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
      override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
      override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
      override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
      override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
      override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
      override implicit def copy: CanCopy[V] = _copy
      override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
      override implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
      override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
      override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
      override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
      override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
    }
}

object MutableInnerProductModule {

  /** Construct a MutableInnerProductModule for the given type from the available implicits */
  def make[V, S](
      implicit _ring: Ring[S],
      _ops: V <:< NumericOps[V],
      _zeroLike: CanCreateZerosLike[V, V],
      _mulVS: OpMulScalar.Impl2[V, S, V],
      _addVV: OpAdd.Impl2[V, V, V],
      _subVV: OpSub.Impl2[V, V, V],
      _dotVV: OpMulInner.Impl2[V, V, S],
      _copy: CanCopy[V],
      _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
      _addIntoVV: OpAdd.InPlaceImpl2[V, V],
      _subIntoVV: OpSub.InPlaceImpl2[V, V],
      _setIntoVV: OpSet.InPlaceImpl2[V, V],
      _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V]): MutableInnerProductModule[V, S] =
    new MutableInnerProductModule[V, S] {
      def scalars: Ring[S] = _ring
      override val hasOps: ConversionOrSubtype[V, NumericOps[V]] = implicitly
      override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
      override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
      override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
      override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
      override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
      override implicit def copy: CanCopy[V] = _copy
      override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
      override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
      override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
      override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
      override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
    }

  implicit def liftFloat[V](implicit vs: MutableInnerProductModule[V, Float]): MutableInnerProductModule[V, Double] =
    new MutableInnerProductModule[V, Double] {
      override def scalars: Field[Double] = Field.fieldDouble

      override def zeroLike: CanCreateZerosLike[V, V] = vs.zeroLike

      override def mulVS: OpMulScalar.Impl2[V, Double, V] = vs.mulVS

      override implicit val hasOps: ConversionOrSubtype[V, NumericOps[V]] = vs.hasOps

      override def close(a: V, b: V, tolerance: Double): Boolean = vs.close(a, b, tolerance)

      override def subVV: OpSub.Impl2[V, V, V] = vs.subVV
      override def addVV: OpAdd.Impl2[V, V, V] = vs.addVV
      override def dotVV: OpMulInner.Impl2[V, V, Double] = vs.dotVV

      override def copy: CanCopy[V] = vs.copy
      override def addIntoVV: OpAdd.InPlaceImpl2[V, V] = vs.addIntoVV
      override def subIntoVV: OpSub.InPlaceImpl2[V, V] = vs.subIntoVV
      override def setIntoVV: OpSet.InPlaceImpl2[V, V] = vs.setIntoVV
      override def mulIntoVS: OpMulScalar.InPlaceImpl2[V, Double] = vs.mulIntoVS
      override def scaleAddVV: scaleAdd.InPlaceImpl3[V, Double, V] = vs.scaleAddVV
    }
}

object MutableVectorField {
  def make[V, S](
      implicit
      _norm: norm.Impl[V, Double],
      _field: Field[S],
      _mulVV: OpMulScalar.Impl2[V, V, V],
      _divVV: OpDiv.Impl2[V, V, V],
      _copy: CanCopy[V],
      _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
      _divIntoVS: OpDiv.InPlaceImpl2[V, S],
      _addIntoVV: OpAdd.InPlaceImpl2[V, V],
      _subIntoVV: OpSub.InPlaceImpl2[V, V],
      _mulIntoVV: OpMulScalar.InPlaceImpl2[V, V],
      _divIntoVV: OpDiv.InPlaceImpl2[V, V],
      _setIntoVV: OpSet.InPlaceImpl2[V, V],
      _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
      _zeroLike: CanCreateZerosLike[V, V],
      _mulVS: OpMulScalar.Impl2[V, S, V],
      _divVS: OpDiv.Impl2[V, S, V],
      _addVV: OpAdd.Impl2[V, V, V],
      _subVV: OpSub.Impl2[V, V, V],
      _neg: OpNeg.Impl[V, V],
      _ops: V <:< NumericOps[V],
      _dotVV: OpMulInner.Impl2[V, V, S]): MutableVectorField[V, S] = new MutableVectorField[V, S] {

    def scalars: Field[S] = _field
    override val hasOps: ConversionOrSubtype[V, NumericOps[V]] = implicitly
    override implicit def normImpl: norm.Impl[V, Double] = _norm
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
    override implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
    override implicit def copy: CanCopy[V] = _copy
    override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    override implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
    override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    override implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV
    override implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def neg: OpNeg.Impl[V, V] = _neg
    override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
  }
}

object MutableLPVectorField {
  def make[V, S](
      implicit
      _norm: norm.Impl[V, Double],
      _norm2: norm.Impl2[V, Double, Double],
      _field: Field[S],
      _mulVV: OpMulScalar.Impl2[V, V, V],
      _divVV: OpDiv.Impl2[V, V, V],
      _copy: CanCopy[V],
      _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
      _divIntoVS: OpDiv.InPlaceImpl2[V, S],
      _addIntoVV: OpAdd.InPlaceImpl2[V, V],
      _subIntoVV: OpSub.InPlaceImpl2[V, V],
      _mulIntoVV: OpMulScalar.InPlaceImpl2[V, V],
      _divIntoVV: OpDiv.InPlaceImpl2[V, V],
      _setIntoVV: OpSet.InPlaceImpl2[V, V],
      _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
      _zeroLike: CanCreateZerosLike[V, V],
      _mulVS: OpMulScalar.Impl2[V, S, V],
      _divVS: OpDiv.Impl2[V, S, V],
      _addVV: OpAdd.Impl2[V, V, V],
      _subVV: OpSub.Impl2[V, V, V],
      _neg: OpNeg.Impl[V, V],
      _ops: V <:< NumericOps[V],
      _dotVV: OpMulInner.Impl2[V, V, S]): MutableLPVectorField[V, S] = new MutableLPVectorField[V, S] {

    def scalars: Field[S] = _field
    override val hasOps: ConversionOrSubtype[V, NumericOps[V]] = implicitly
    override implicit def normImpl: norm.Impl[V, Double] = _norm
    override implicit def normImpl2: norm.Impl2[V, Double, Double] = _norm2
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
    override implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
    override implicit def copy: CanCopy[V] = _copy
    override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    override implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
    override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    override implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV
    override implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def neg: OpNeg.Impl[V, V] = _neg
    override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
  }
}

object MutableCoordinateField {
  def make[V, S](
      implicit
      _ops: V <:< NumericOps[V],
      _normImpl2: norm.Impl2[V, Double, Double],
      _norm: norm.Impl[V, Double],
      _field: Field[S],
      _mulVV: OpMulScalar.Impl2[V, V, V],
      _divVV: OpDiv.Impl2[V, V, V],
      _copy: CanCopy[V],
      _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
      _divIntoVS: OpDiv.InPlaceImpl2[V, S],
      _addIntoVV: OpAdd.InPlaceImpl2[V, V],
      _subIntoVV: OpSub.InPlaceImpl2[V, V],
      _mulIntoVV: OpMulScalar.InPlaceImpl2[V, V],
      _divIntoVV: OpDiv.InPlaceImpl2[V, V],
      _setIntoVV: OpSet.InPlaceImpl2[V, V],
      _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
      _zeroLike: CanCreateZerosLike[V, V],
      _mulVS: OpMulScalar.Impl2[V, S, V],
      _divVS: OpDiv.Impl2[V, S, V],
      _addVV: OpAdd.Impl2[V, V, V],
      _subVV: OpSub.Impl2[V, V, V],
      _neg: OpNeg.Impl[V, V],
      _dotVV: OpMulInner.Impl2[V, V, S],
      _zipMapVals: CanZipMapValues[V, S, S, V],
      _traverseVals: CanTraverseValues[V, S],
      _mapVals: CanMapValues[V, S, S, V],
      _scalarOf: ScalarOf[V, S]): MutableCoordinateField[V, S] = new MutableCoordinateField[V, S] {

    def scalars: Field[S] = _field
    override val hasOps: ConversionOrSubtype[V, NumericOps[V]] = implicitly
    override implicit def normImpl: norm.Impl[V, Double] = _norm
    override implicit def normImpl2: norm.Impl2[V, Double, Double] = _normImpl2
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
    override implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
    override implicit def copy: CanCopy[V] = _copy
    override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    override implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
    override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    override implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV
    override implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def neg: OpNeg.Impl[V, V] = _neg
    override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
    override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals
    override implicit def scalarOf: ScalarOf[V, S] = _scalarOf
    override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
    override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals
  }
}

object MutableFiniteCoordinateField {
  def make[V, I, S](
      implicit
      _norm2: norm.Impl2[V, Double, Double],
      _norm: norm.Impl[V, Double],
      _field: Field[S],
      _addVS: OpAdd.Impl2[V, S, V],
      _subVS: OpSub.Impl2[V, S, V],
      _mulVV: OpMulScalar.Impl2[V, V, V],
      _divVV: OpDiv.Impl2[V, V, V],
      _copy: CanCopy[V],
      _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
      _divIntoVS: OpDiv.InPlaceImpl2[V, S],
      _addIntoVV: OpAdd.InPlaceImpl2[V, V],
      _subIntoVV: OpSub.InPlaceImpl2[V, V],
      _addIntoVS: OpAdd.InPlaceImpl2[V, S],
      _subIntoVS: OpSub.InPlaceImpl2[V, S],
      _mulIntoVV: OpMulScalar.InPlaceImpl2[V, V],
      _divIntoVV: OpDiv.InPlaceImpl2[V, V],
      _setIntoVV: OpSet.InPlaceImpl2[V, V],
      _setIntoVS: OpSet.InPlaceImpl2[V, S],
      _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
      _zeroLike: CanCreateZerosLike[V, V],
      _zero: CanCreateZeros[V, I],
      _dim: dim.Impl[V, I],
      _mulVS: OpMulScalar.Impl2[V, S, V],
      _divVS: OpDiv.Impl2[V, S, V],
      _addVV: OpAdd.Impl2[V, V, V],
      _subVV: OpSub.Impl2[V, V, V],
      _neg: OpNeg.Impl[V, V],
      _ops: V <:< NumericOps[V] with QuasiTensor[I, S],
      _dotVV: OpMulInner.Impl2[V, V, S],
      _zipMapVals: CanZipMapValues[V, S, S, V],
      _zipMapKeyVals: CanZipMapKeyValues[V, I, S, S, V],
      _traverseVals: CanTraverseValues[V, S],
      _mapVals: CanMapValues[V, S, S, V],
      _scalarOf: ScalarOf[V, S]): MutableFiniteCoordinateField[V, I, S] = new MutableFiniteCoordinateField[V, I, S] {
    def scalars: Field[S] = _field

    override val hasOps: ConversionOrSubtype[V, NumericOps[V] with QuasiTensor[I, S]] = implicitly
    override implicit def normImpl: norm.Impl[V, Double] = _norm
    override implicit def normImpl2: norm.Impl2[V, Double, Double] = _norm2
    override implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def subVS: OpSub.Impl2[V, S, V] = _subVS
    override implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
    override implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
    override implicit def copy: CanCopy[V] = _copy
    override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    override implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
    override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    override implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S] = _addIntoVS
    override implicit def subIntoVS: OpSub.InPlaceImpl2[V, S] = _subIntoVS
    override implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV
    override implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def neg: OpNeg.Impl[V, V] = _neg
    override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    override implicit def setIntoVS: OpSet.InPlaceImpl2[V, S] = _setIntoVS
    override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
    override implicit def scalarOf: ScalarOf[V, S] = _scalarOf
    override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals
    override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
    override implicit def zipMapKeyValues: CanZipMapKeyValues[V, I, S, S, V] = _zipMapKeyVals
    override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals
    override implicit def zero: CanCreateZeros[V, I] = _zero
    override implicit def canDim: dim.Impl[V, I] = _dim
  }

//  implicit def liftFloat[K, V](
//      implicit vs: MutableFiniteCoordinateField[V, K, Float]): MutableFiniteCoordinateField[V, K, Double] =
//    new MutableFiniteCoordinateField[V, K, Double] {
//      override def scalars: Field[Double] = Field.fieldDouble
//
//      override def zeroLike: CanCreateZerosLike[V, V] = vs.zeroLike
//
//      override def mulVS: OpMulScalar.Impl2[V, Double, V] = ???
//
//      // TODO: this will cause problems..
//      override implicit val hasOps: NumericOps[V] with QuasiTensor[K, Double] =
//        vs.hasOps(v).asInstanceOf[NumericOps[V] with QuasiTensor[K, Double]]
//
//      override def close(a: V, b: V, tolerance: Double): Boolean = vs.close(a, b, tolerance)
//
//      override def subVV: OpSub.Impl2[V, V, V] = vs.subVV
//      override def addVV: OpAdd.Impl2[V, V, V] = vs.addVV
//      override def dotVV: OpMulInner.Impl2[V, V, Double] = vs.dotVV
//
//      override def copy: CanCopy[V] = vs.copy
//      override def addIntoVV: OpAdd.InPlaceImpl2[V, V] = vs.addIntoVV
//      override def subIntoVV: OpSub.InPlaceImpl2[V, V] = vs.subIntoVV
//      override def setIntoVV: OpSet.InPlaceImpl2[V, V] = vs.setIntoVV
//      override def mulIntoVS: OpMulScalar.InPlaceImpl2[V, Double] = vs.mulIntoVS
//      override def scaleAddVV: scaleAdd.InPlaceImpl3[V, Double, V] = vs.scaleAddVV
//      override def addIntoVS: OpAdd.InPlaceImpl2[V, Double] = vs.addIntoVS
//      override def subIntoVS: OpSub.InPlaceImpl2[V, Double] = vs.subIntoVS
//      override def setIntoVS: OpSet.InPlaceImpl2[V, Double] = vs.setIntoVS
//      override def divVV: OpDiv.Impl2[V, V, V] = vs.divVV
//      override def divVS: OpDiv.Impl2[V, Double, V] = vs.divVS
//      override def mulVV: OpMulScalar.Impl2[V, V, V] = vs.mulVV
//      override def neg: OpNeg.Impl[V, V] = vs.neg
//      override def normImpl2: norm.Impl2[V, Double, Double] = vs.normImpl2
//      override def divIntoVV: OpDiv.InPlaceImpl2[V, V] = vs.divIntoVV
//      override def zero: CanCreateZeros[V, K] = vs.zero
//      override def canDim: dim.Impl[V, K] = vs.canDim
//      override def addVS: OpAdd.Impl2[V, Double, V] = vs.addVS
//      override def subVS: OpSub.Impl2[V, Double, V] = vs.subVS
//      override def divIntoVS: OpDiv.InPlaceImpl2[V, Double] = vs.divIntoVS
//
//      override implicit def scalarOf: ScalarOf[V, Double] = ScalarOf.dummy
//
//      override implicit def mapActiveValues: CanMapActiveValues[V, Double, Double, V] =
//        new CanMapActiveValues[V, Double, Double, V] {
//          override def apply(v: V, v2: Double => Double): V = vs.mapActiveValues(v, a => v2(a).toFloat)
//        }
//
//      override implicit def mapValues: CanMapValues[V, Double, Double, V] = new CanMapValues[V, Double, Double, V] {
//        override def apply(v: V, v2: Double => Double): V = vs.mapValues(v, a => v2(a).toFloat)
//      }
//
//      override implicit def zipMapValues: CanZipMapValues[V, Double, Double, V] =
//        new CanZipMapValues[V, Double, Double, V] {
//          override def map(from: V, from2: V, fn: (Double, Double) => Double): V =
//            vs.zipMapValues.map(from, from2, (a, b) => fn(a, b).toFloat)
//        }
//
//      override def zipMapKeyValues: CanZipMapKeyValues[V, K, Double, Double, V] =
//        new CanZipMapKeyValues[V, K, Double, Double, V] {
//          override def map(from: V, from2: V, fn: (K, Double, Double) => Double): V =
//            vs.zipMapKeyValues.map(from, from2, (k, a, b) => fn(k, a, b).toFloat)
//
//          override def mapActive(from: V, from2: V, fn: (K, Double, Double) => Double): V =
//            vs.zipMapKeyValues.mapActive(from, from2, (k, a, b) => fn(k, a, b).toFloat)
//        }
//
//      override implicit def iterateValues: CanTraverseValues[V, Double] = new CanTraverseValues[V, Double] {
//
//        /** Traverses all values from the given collection. */
//        override def traverse(from: V, fn: CanTraverseValues.ValuesVisitor[Double]): Unit =
//          vs.iterateValues.traverse(
//            from,
//            new CanTraverseValues.ValuesVisitor[Float] {
//              override def visit(a: Float): Unit = fn.visit(a)
//
//              override def zeros(numZero: Int, zeroValue: Float): Unit = fn.visit(zeroValue)
//            }
//          )
//
//        override def isTraversableAgain(from: V): Boolean = vs.iterateValues.isTraversableAgain(from)
//      }
//
//      override implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = vs.mulIntoVV
//    }
}

object MutableEnumeratedCoordinateField {
  def make[V, I, S](
      implicit
      _norm2: norm.Impl2[V, Double, Double],
      _norm: norm.Impl[V, Double],
      _field: Field[S],
      _mulVV: OpMulScalar.Impl2[V, V, V],
      _divVV: OpDiv.Impl2[V, V, V],
      _copy: CanCopy[V],
      _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
      _divIntoVS: OpDiv.InPlaceImpl2[V, S],
      _addIntoVV: OpAdd.InPlaceImpl2[V, V],
      _subIntoVV: OpSub.InPlaceImpl2[V, V],
      _mulIntoVV: OpMulScalar.InPlaceImpl2[V, V],
      _divIntoVV: OpDiv.InPlaceImpl2[V, V],
      _setIntoVV: OpSet.InPlaceImpl2[V, V],
      _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
      _zeroLike: CanCreateZerosLike[V, V],
      _mulVS: OpMulScalar.Impl2[V, S, V],
      _divVS: OpDiv.Impl2[V, S, V],
      _addVV: OpAdd.Impl2[V, V, V],
      _subVV: OpSub.Impl2[V, V, V],
      _neg: OpNeg.Impl[V, V],
      _ops: V <:< NumericOps[V] with QuasiTensor[I, S],
      _dotVV: OpMulInner.Impl2[V, V, S],
      _zipMapVals: CanZipMapValues[V, S, S, V],
      _zipMapKeyVals: CanZipMapKeyValues[V, I, S, S, V],
      _traverseVals: CanTraverseValues[V, S],
      _mapVals: CanMapValues[V, S, S, V],
      _scalarOf: ScalarOf[V, S]): MutableEnumeratedCoordinateField[V, I, S] =
    new MutableEnumeratedCoordinateField[V, I, S] {
      def scalars: Field[S] = _field

      override val hasOps: ConversionOrSubtype[V, NumericOps[V] with QuasiTensor[I, S]] = implicitly
      override implicit def normImpl: norm.Impl[V, Double] = _norm
      override implicit def normImpl2: norm.Impl2[V, Double, Double] = _norm2
      override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
      override implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
      override implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
      override implicit def copy: CanCopy[V] = _copy
      override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
      override implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
      override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
      override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
      override implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV
      override implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV
      override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
      override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
      override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
      override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
      override implicit def neg: OpNeg.Impl[V, V] = _neg
      override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
      override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
      override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
      override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals
      override implicit def scalarOf: ScalarOf[V, S] = _scalarOf
      override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
      override implicit def zipMapKeyValues: CanZipMapKeyValues[V, I, S, S, V] = _zipMapKeyVals
      override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals
    }
}

object MutableOptimizationSpace {

  object SparseFieldOptimizationSpace {
    implicit def sparseOptSpace[S: Field: Zero: ClassTag]
      : MutableOptimizationSpace[CSCMatrix[S], SparseVector[S], S] = {
      implicitly[OpNeg.Impl[SparseVector[S], SparseVector[S]]]
      implicitly[OpNeg.Impl[SparseVector[S], SparseVector[S]]]
      type T = SparseVector[S]
      type V = S
      type U = SparseVector[S]
      implicitly[ScalarOf[T, V]]
      implicitly[Ring[V]]
      implicitly[OpMulScalar.Impl2[T, V, U]]
      val norms = EntrywiseMatrixNorms.make[CSCMatrix[S], S]
      import norms._
      make[CSCMatrix[S], SparseVector[S], S](_.asCscRow, _.flatten())
    }
  }

  object DenseFieldOptimizationSpace {
    implicit def denseOptSpace[S: Field: ClassTag]: MutableOptimizationSpace[DenseMatrix[S], DenseVector[S], S] = {
      val norms = EntrywiseMatrixNorms.make[DenseMatrix[S], S]
//      import DenseMatrix.canMapValues
      import norms._
      make[DenseMatrix[S], DenseVector[S], S](_.asDenseMatrix, _.flatten())
    }
  }

  object DenseDoubleOptimizationSpace {
    implicit def denseDoubleOptSpace: MutableOptimizationSpace[DenseMatrix[Double], DenseVector[Double], Double] = {
      val norms = EntrywiseMatrixNorms.make[DenseMatrix[Double], Double]
//      import DenseMatrix.canMapValues
      import norms.{canInnerProduct, canNorm_Double}
      make[DenseMatrix[Double], DenseVector[Double], Double](_.asDenseMatrix, _.flatten())
    }
  }

  object SparseDoubleOptimizationSpace {
    implicit def sparseDoubleOptSpace: MutableOptimizationSpace[CSCMatrix[Double], SparseVector[Double], Double] = {
      val norms = EntrywiseMatrixNorms.make[CSCMatrix[Double], Double]
      import norms.{canInnerProduct, canNorm_Double}
      make[CSCMatrix[Double], SparseVector[Double], Double](_.asCscRow, _.flatten())
    }
  }

  def make[M, V, S](toMat: V => M, toVec: M => V)(
      implicit
      _norm2: norm.Impl2[V, Double, Double],
      _norm: norm.Impl[V, Double],
      _field: Field[S],
      _mulMSMat: OpMulMatrix.Impl2[M, S, M],
      _addVS: OpAdd.Impl2[V, S, V],
      _subVS: OpSub.Impl2[V, S, V],
      _mulVV: OpMulScalar.Impl2[V, V, V],
      _divVV: OpDiv.Impl2[V, V, V],
      _copy: CanCopy[V],
      _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
      _divIntoVS: OpDiv.InPlaceImpl2[V, S],
      _addIntoVV: OpAdd.InPlaceImpl2[V, V],
      _subIntoVV: OpSub.InPlaceImpl2[V, V],
      _addIntoVS: OpAdd.InPlaceImpl2[V, S],
      _subIntoVS: OpSub.InPlaceImpl2[V, S],
      _mulIntoVV: OpMulScalar.InPlaceImpl2[V, V],
      _divIntoVV: OpDiv.InPlaceImpl2[V, V],
      _setIntoVV: OpSet.InPlaceImpl2[V, V],
      _setIntoVS: OpSet.InPlaceImpl2[V, S],
      _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
      _zeroLike: CanCreateZerosLike[V, V],
      _zero: CanCreateZeros[V, Int],
      _dim: dim.Impl[V, Int],
      _mulVS: OpMulScalar.Impl2[V, S, V],
      _divVS: OpDiv.Impl2[V, S, V],
      _addVV: OpAdd.Impl2[V, V, V],
      _subVV: OpSub.Impl2[V, V, V],
      _neg: OpNeg.Impl[V, V],
      _ops: V <:< NumericOps[V] with QuasiTensor[Int, S],
      _dotVV: OpMulInner.Impl2[V, V, S],
      _zipMapVals: CanZipMapValues[V, S, S, V],
      _traverseVals: CanTraverseValues[V, S],
      _mapVals: CanMapValues[V, S, S, V],
      _scalarOf: ScalarOf[V, S],
      _norm2M: norm.Impl2[M, Double, Double],
      _normM: norm.Impl[M, Double],
      _addMS: OpAdd.Impl2[M, S, M],
      _subMS: OpSub.Impl2[M, S, M],
      _mulMM: OpMulScalar.Impl2[M, M, M],
      _divMM: OpDiv.Impl2[M, M, M],
      _copyM: CanCopy[M],
      _mulIntoMS: OpMulScalar.InPlaceImpl2[M, S],
      _divIntoMS: OpDiv.InPlaceImpl2[M, S],
      _addIntoMM: OpAdd.InPlaceImpl2[M, M],
      _subIntoMM: OpSub.InPlaceImpl2[M, M],
      _addIntoMS: OpAdd.InPlaceImpl2[M, S],
      _subIntoMS: OpSub.InPlaceImpl2[M, S],
      _mulIntoMM: OpMulScalar.InPlaceImpl2[M, M],
      _divIntoMM: OpDiv.InPlaceImpl2[M, M],
      _setIntoMM: OpSet.InPlaceImpl2[M, M],
      _setIntoMS: OpSet.InPlaceImpl2[M, S],
      _scaleAddMSM: scaleAdd.InPlaceImpl3[M, S, M],
      _zeroLikeM: CanCreateZerosLike[M, M],
      _zeroM: CanCreateZeros[M, (Int, Int)],
      _dimM: dim.Impl[M, (Int, Int)],
      _mulMS: OpMulScalar.Impl2[M, S, M],
      _divMS: OpDiv.Impl2[M, S, M],
      _addMM: OpAdd.Impl2[M, M, M],
      _subMM: OpSub.Impl2[M, M, M],
      _negM: OpNeg.Impl[M, M],
      _opsM: M <:< NumericOps[M] with QuasiTensor[(Int, Int), S],
      _dotMM: OpMulInner.Impl2[M, M, S],
      _zipMapValsM: CanZipMapValues[M, S, S, M],
      _zipMapKeyVals: CanZipMapKeyValues[V, Int, S, S, V],
      _traverseValsM: CanTraverseValues[M, S],
      _mapValsM: CanMapValues[M, S, S, M],
      _scalarOfM: ScalarOf[M, S],
      _mulMMM: OpMulMatrix.Impl2[M, M, M],
      _mulMVV: OpMulMatrix.Impl2[M, V, V],
      _mulVTM: OpMulMatrix.Impl2[V, Transpose[V], M],
      _canTrans: CanTranspose[V, Transpose[V]]): MutableOptimizationSpace[M, V, S] =
    new MutableOptimizationSpace[M, V, S] {
      def toMatrix(v: V): M = toMat(v)
      def toVector(m: M): V = toVec(m)

      def closeM(a: M, b: M, tolerance: Double): Boolean =
        normM(subMM(a, b)) <= tolerance * math.max(normM(a), normM(b))

      implicit def fieldNorm: norm.Impl[S, Double] = _field.normImpl
      implicit def setIntoMM: OpSet.InPlaceImpl2[M, M] = _setIntoMM
      implicit def mulMSMat: OpMulMatrix.Impl2[M, S, M] = _mulMSMat
      implicit def divMS: OpDiv.Impl2[M, S, M] = _divMS
      implicit def normMImpl2: norm.Impl2[M, Double, Double] = _norm2M
      implicit def normM: norm.Impl[M, Double] = _normM
      implicit def divMM: OpDiv.Impl2[M, M, M] = _divMM
      implicit def zeroLikeM: CanCreateZerosLike[M, M] = _zeroLikeM
      implicit def scaleAddMM: scaleAdd.InPlaceImpl3[M, S, M] = _scaleAddMSM
      implicit def addIntoMS: OpAdd.InPlaceImpl2[M, S] = _addIntoMS
      implicit def negM: OpNeg.Impl[M, M] = _negM
      implicit def subIntoMS: OpSub.InPlaceImpl2[M, S] = _subIntoMS
      implicit def addIntoMM: OpAdd.InPlaceImpl2[M, M] = _addIntoMM
      implicit def canDimM: dim.Impl[M, (Int, Int)] = _dimM
      implicit def subIntoMM: OpSub.InPlaceImpl2[M, M] = _subIntoMM
      implicit def copyM: CanCopy[M] = _copyM
      implicit def mulMS: OpMulScalar.Impl2[M, S, M] = _mulMS
      implicit def dotMM: OpMulInner.Impl2[M, M, S] = _dotMM
      implicit def subMS: OpSub.Impl2[M, S, M] = _subMS
      implicit def mulMMS: OpMulScalar.Impl2[M, M, M] = _mulMM
      implicit def mulIntoMS: OpMulScalar.InPlaceImpl2[M, S] = _mulIntoMS
      implicit def subMM: OpSub.Impl2[M, M, M] = _subMM
      implicit def divIntoMM: OpDiv.InPlaceImpl2[M, M] = _divIntoMM
      implicit def mulIntoMM: OpMulScalar.InPlaceImpl2[M, M] = _mulIntoMM
      def hasMOps: ConversionOrSubtype[M, NumericOps[M] with QuasiTensor[(Int, Int), S]] = implicitly
      implicit def zeroM: CanCreateZeros[M, (Int, Int)] = _zeroM
      implicit def addMM: OpAdd.Impl2[M, M, M] = _addMM
      implicit def divIntoMS: OpDiv.InPlaceImpl2[M, S] = _divIntoMS
      implicit def addMS: OpAdd.Impl2[M, S, M] = _addMS
      implicit def setIntoMS: OpSet.InPlaceImpl2[M, S] = _setIntoMS
      val hasOps: ConversionOrSubtype[V, NumericOps[V] with QuasiTensor[Int, S]] = implicitly
      implicit def neg: OpNeg.Impl[V, V] = _neg
      implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
      implicit def canDim: dim.Impl[V, Int] = _dim
      implicit def zero: CanCreateZeros[V, Int] = _zero
      implicit def normImpl2: norm.Impl2[V, Double, Double] = _norm2
      implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV
      implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV
      implicit def setIntoVS: OpSet.InPlaceImpl2[V, S] = _setIntoVS
      implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
      override implicit def zipMapKeyValues: CanZipMapKeyValues[V, Int, S, S, V] = _zipMapKeyVals
      implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals
      implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals

      implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
      implicit def subVS: OpSub.Impl2[V, S, V] = _subVS
      implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
      implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
      implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
      implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS
      implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
      implicit def scalars: Field[S] = _field
      implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
      implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
      implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
      implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
      implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S] = _addIntoVS
      implicit def subIntoVS: OpSub.InPlaceImpl2[V, S] = _subIntoVS
      implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
      implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
      implicit def copy: CanCopy[V] = _copy
      implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
      implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
      implicit def mapValuesM: CanMapValues[M, S, S, M] = _mapValsM
      implicit def iterateValuesM: CanTraverseValues[M, S] = _traverseValsM
      implicit def zipMapValuesM: CanZipMapValues[M, S, S, M] = _zipMapValsM
      implicit def mulMMM: OpMulMatrix.Impl2[M, M, M] = _mulMMM
//    implicit def mulMVM: OpMulMatrix.Impl2[M, V, M] = _mulMVM
//    implicit def mulVMM: OpMulMatrix.Impl2[V, M, M] = _mulVMM
      implicit def mulMVV: OpMulMatrix.Impl2[M, V, V] = _mulMVV
      implicit def mulVTM: OpMulMatrix.Impl2[V, Transpose[V], M] = _mulVTM
      implicit def canTrans: CanTranspose[V, Transpose[V]] = _canTrans

      override implicit def scalarOfM: ScalarOf[M, S] = _scalarOfM

      override implicit def scalarOf: ScalarOf[V, S] = _scalarOf
    }
}

private object FloatDoubleOperatorAdaptors {
  implicit def liftOp2[Op <: UFunc, V, R](op: UImpl2[Op, V, Float, R]): UImpl2[Op, V, Double, R] = {
    new UImpl2[Op, V, Double, R] {
      override def apply(v: V, v2: Double): R = op(v, v2.toFloat)
    }
  }

  implicit def liftInPlaceOp2[Op <: UFunc, V](op: InPlaceImpl2[Op, V, Float]): InPlaceImpl2[Op, V, Double] = {
    new InPlaceImpl2[Op, V, Double] {
      override def apply(v: V, v2: Double): Unit = op(v, v2.toFloat)
    }
  }

  implicit def liftInPlaceOp3[Op <: UFunc, V, V3](
      op: InPlaceImpl3[Op, V, Float, V3]): InPlaceImpl3[Op, V, Double, V3] = {
    new InPlaceImpl3[Op, V, Double, V3] {
      override def apply(v: V, v2: Double, v3: V3): Unit = op(v, v2.toFloat, v3)
    }
  }

  implicit def liftOpReturnFloat[Op <: UFunc, V](op: UImpl2[Op, V, V, Float]): UImpl2[Op, V, V, Double] = {
    new UImpl2[Op, V, V, Double] {
      override def apply(v: V, v2: V): Double = op(v, v2)
    }
  }
}
